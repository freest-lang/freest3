---------------------------------- Util.fst ----------------------------------
-- module Util where

sink : forall a . a -> ()
sink _ = ()

---------------------------------- Concurrent.fst ----------------------------------
-- module Concurrent where

-- | Receive a value from a shared channel
receiveUn : forall a:TL . *?a -> a
receiveUn ch = fst[a, *?a] $ receive ch

-- | Send a value in a shared channel
sendUn : forall a:TL . a -> *!a -o ()
sendUn x ch = let _ = send x ch in ()

-- | Create a new child process and a linear channel through which it can 
--   communicate with its parent process.
forkWith : forall a:SL . (dualof a -o ()) -> a
forkWith f =
    let (c, s) = new a in
    fork $ f s;
    c


initSession : forall a:SL . *!a -> dualof a
initSession ch =
    let (c, s) = new a in
    sendUn[a] c ch;
    s

runServer : forall a:SL b . *!a -> (b -> dualof a -o b) -> b -> ()
runServer ch handle state =
    runServer[a, b] ch handle $ handle state $ initSession[a] ch 

---------------------------------- SharedCounter ----------------------------------

type Counter : SU = *?Int

initCounter : Counter
initCounter = 
    forkWith[Counter] (\ch:*!Int -o runCounter 0 ch)

runCounter : Int -> dualof Counter -> ()
runCounter i counter =
    sendUn[Int] i counter;
    runCounter (i+1) counter

---------------------------------- SharedQueue ----------------------------------

{- channel types -}

-- type Head : SU = {- Dequeue -} *?Int
-- type Tail : SU = {- Enqueue -} dualof Head

-- type Internal : SL = ?Int; ?Internal

{- nodes -}

runHeadNode : forall a:TL . (rec x:SL . ?a; ?x) -> dualof *?a -o ()
runHeadNode prev head = 
    -- receive value & next node endpoint
    let (i, prev) = receive prev in
    let (prev, _) = receive prev in
    -- send value to client
    sendUn [a] i head;
    -- run node with new endpoint
    runHeadNode[a] prev head

runTailNode : forall a:TL . dualof (rec x:SL . ?a; ?x) -> dualof *!a -o ()
runTailNode next tail =
    let i = receiveUn [a] tail in
    let next' = 
        forkWith[dualof (rec x:SL . ?a; ?x)] (\c:(rec x:SL . ?a; ?x) -o sink[Skip] $ send c $ send i next) 
        in
    runTailNode[a] next' tail 

{- queue -}

initQueue : forall a:TL . () -> (*?a, *!a)
initQueue _ =
    let (internalC, internalS) = new (rec x:SL . ?a; ?x) in
    ( forkWith [*?a] (runHeadNode[a] internalC)
    , forkWith [*!a] (runTailNode[a] internalS)
    )

enqueue : forall a:TL . a -> (*?a, *!a) -o ()
enqueue i queue = 
    sendUn [a] i $ snd[*?a, *!a] queue

dequeue : forall a:TL . (*?a, *!a) -> a
dequeue queue = 
    receiveUn[a] $ fst[*?a, *!a] queue

---------------------------------- SharedList ----------------------------------

{- list structure (while there's no native one) -}

data List = Nil 
          | Cons (ProductId, Issue, RmaNumber) List

{- channel types -}

type SharedList : SU = *?ListC
type ListC : SL = +{ Append: !ProductId; !Issue; !RmaNumber; ListC
                   , Close: Skip 
                   }

{- list server -}

runListServer : dualof SharedList -> ()
runListServer ch =
    runServer[ListC, List] ch runListService Nil 

runListService : List -> dualof ListC -o List
runListService list ch = 
    match ch with {
        Append ch -> 
            let (productId, ch) = receive ch in
            let (issue    , ch) = receive ch in
            let (rmaNumber, ch) = receive ch in
            runListService (Cons (productId, issue, rmaNumber) list) ch,
        Close _ -> 
            list
    }

{- client functions -}

append : SharedList -> (ProductId, Issue, RmaNumber) -> ()
append ch triple =
    let (productId, pair) = triple in
    let (issue, rmaNumber) = pair in
    receiveUn[ListC] ch &
    select Append &
    send productId &
    send issue &
    send rmaNumber &
    select Close &
    sink[Skip]

---------------------------------- SharedMap ----------------------------------

{- types for the map (while there's no type ops) -}

type ProductName = Char
type Amount      = Int
type Price       = Int

{- map structure -}

data Map = Empty 
         | Entry ProductName (Amount, Price) Map

data MaybeValue = NothingValue | JustValue (Amount, Price)

mapPut : ProductName -> (Amount, Price) -> Map -> Map
mapPut pName val map =
    case map of {
        Empty -> Entry pName val Empty,
        Entry pName' val' map' ->
            if ord pName == ord pName'
            then
                -- if already exist, replace value 
                Entry pName' val map'
            else
                -- else, recur
                Entry pName' val' $ mapPut pName val map'
    }

mapGet : ProductName -> Map -> MaybeValue
mapGet pName map =
    case map of {
        Empty -> NothingValue,
        Entry pName' val' map' ->
            if ord pName == ord pName'
            then JustValue val'
            else mapGet pName map'
    }

mapHas : ProductName -> Map -> Bool
mapHas pName map = 
    case map of {
        Empty -> False,
        Entry pName' _ map' ->
            if ord pName == ord pName'
            then True
            else mapHas pName map'
    }

{- channel types -}

type SharedMap : SU = *?MapC
type MapC : SL = +{ Put: !ProductName; ValueC     ; MapC
                  , Get: !ProductName; MaybeValueC; MapC
                  , Has: !ProductName; ?Bool      ; MapC
                  , Close: Skip
                  }

type ValueC : SL = !Amount; !Price

type MaybeValueC : SL = &{ JustVal: dualof ValueC
                         , NothingVal: Skip
                         }

{- map server -}

runMapServer : dualof SharedMap -> ()
runMapServer ch = 
    runServer[MapC, Map] ch runMapService Empty

runMapService : Map -> dualof MapC -o Map
runMapService map ch =
    match ch with {
        Put ch ->
            let (pName , ch) = receive ch in
            let (amount, ch) = receive ch in
            let (price , ch) = receive ch in
            runMapService (mapPut pName (amount, price) map) ch,
        Get ch -> 
            let (pName , ch) = receive ch in
            let maybeVal = mapGet pName map in
            let ch = case maybeVal of {
                        NothingValue  -> select NothingVal ch,
                        JustValue val -> 
                            let (amount, price) = val in
                            select JustVal ch & 
                            send amount &
                            send price
                     } in
            runMapService map ch,
        Has ch ->
            let (pName , ch) = receive ch in
            runMapService map $ send (mapHas pName map) ch,
        Close _ -> map
    }

{- client functions -}

put : SharedMap -> ProductName -> (Amount, Price) -> ()
put ch pName val =
    let (amount, price) = val in
    receiveUn[MapC] ch &
    select Put &
    send pName &
    send amount &
    send price &
    select Close &
    sink[Skip]

get : SharedMap -> ProductName -> MaybeValue
get ch pName =
    let ch = receiveUn[MapC] ch &
             select Get &
             send pName in
    match ch with {
        JustVal ch -> 
            let (amount, ch) = receive ch in
            let (price, ch)  = receive ch in
            sink[Skip] $ select Close ch;
            JustValue (amount, price),
        NothingVal ch -> 
            sink[Skip] $ select Close ch;
            NothingValue
    }

has : SharedMap -> ProductName -> Bool
has ch pName = 
    let (b, ch) = receiveUn[MapC] ch &
                  select Has &
                  send pName &
                  receive in
    sink[Skip] $ select Close ch;
    b

---------------------------------- Bank ----------------------------------

{- channel types -}

type Bank : SU = *?BankService
type BankService : SL = {- CreatePayment: -} !Price; ?PaymentC

type PaymentC : SL = !CCNumber; !CCCode

{- bank worker -}

bankWorker : dualof Bank -> ()
bankWorker ch =
    runServer[BankService, ()] ch runBankService ()

runBankService : () -> dualof BankService -o ()
runBankService _ ch =
    let ch = snd[Int, !PaymentC] $ receive ch in
    let (c, s) = new PaymentC in
    sink[Skip] $ send c ch;
    runPayment s

runPayment : dualof PaymentC -> ()
runPayment ch =
    -- mock up
    let (_, ch) = receive ch in
    let (_, ch) = receive ch in
    ()

{- client functions -}

createPayment : Bank -> Price -> PaymentC
createPayment bank price =
    let ch = receiveUn[BankService] bank in
    fst[PaymentC, Skip] $ receive $ send price ch


---------------------------------- Tech Store ----------------------------------

type ProductId = Int
type Issue = String
type RmaNumber = Int

type CCNumber = Int
type CCCode = Int

{- channel types -}

type TechStore   : SU = *?TechService
type TechService : SL = +{ Buy: ?BuyC
                         , Rma: ?RmaC
                         }

type BuyC : SL = !ProductName; AvailabilityC
type RmaC : SL = !ProductId; !Issue; ?RmaNumber

type AvailabilityC : SL = &{ Available : ?Price; CheckoutC
                           , OutOfStock: Skip
                           }

type CheckoutC : SL = +{ Confirm: PaymentC
                       , Cancel : Skip
                       }


{- store front -}

type BuyQueue = (*?dualof BuyC, *!dualof BuyC)
type RmaQueue = (*?dualof RmaC, *!dualof RmaC)

runStoreFront : dualof TechStore -> BuyQueue -> RmaQueue -> ()
runStoreFront store buyQueue rmaQueue =
    match initSession[TechService] store with {
        Buy ch ->
            let (c, s) = new BuyC in
            let _ = send c ch in
            enqueue[dualof BuyC] s buyQueue;
            runStoreFront store buyQueue rmaQueue,
        Rma ch ->
            let (c, s) = new RmaC in
            let _ = send c ch in
            enqueue[dualof RmaC] s rmaQueue;
            runStoreFront store buyQueue rmaQueue
    }

{- buy workers -}

-- buyWorker : BuyQueue -> SharedMap -> Bank -> ()
-- buyWorker buyQueue map bank = 
--     let ch = dequeue[dualof BuyC] buyQueue in
--     --
--     let (pName, ch) = receive ch in
--     -- manually work with map to prevent interleaving
--     let mapS = receiveUn[MapC] map in
    

--     if isNothingValue
--     then
--         sink[Skip] $  select OutOfStock ch
--     else
--         let ch = send select Available ch in
        

--     --
--     buyWorker buyQueue map bank



{- rma workers -}

rmaWorker : RmaQueue -> Counter -> SharedList -> ()
rmaWorker rmaQueue counter list =
    let ch = dequeue[dualof RmaC] rmaQueue in
    --
    let (productId, ch) = receive ch in
    let (issue    , ch) = receive ch in
    let rmaNumber = receiveUn[Int] counter in
    append list (productId, issue, rmaNumber);
    sink[Skip] $ send rmaNumber ch;
    --
    rmaWorker rmaQueue counter list



{- main -}

-- main : Int
-- main = 
--     let intQueue = initQueue[Int] () in
--     enqueue[Int] 1 intQueue;
--     dequeue[Int] intQueue