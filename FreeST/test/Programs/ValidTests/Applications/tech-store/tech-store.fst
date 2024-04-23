---------------------------------- SharedCounter ----------------------------------

type Counter = *?Int

initCounter : Counter
initCounter = 
    forkWith @Counter @() (\ch:*!Int 1-> runCounter 0 ch)

runCounter : Int -> dualof Counter -> ()
runCounter i counter =
    send_ @Int i counter;
    runCounter (i+1) counter

---------------------------------- SharedQueue ----------------------------------

{- channel types -}

-- type Head : *S = {- Dequeue -} *?Int
-- type Tail : *S = {- Enqueue -} dualof Head

-- type Internal = ?Int; ?Internal

{- nodes -}

runHeadNode : (rec x . ?a; ?x; Close) -> dualof *?a 1-> ()
runHeadNode prev head = 
    -- receive value |> next node endpoint
    let (i, prev) = receive prev in
    let (prev', prev) = receive prev in
    close prev;
    -- send value to client
    send_ @a i head;
    -- run node with new endpoint
    runHeadNode @a prev' head

runTailNode : dualof (rec x . ?a; ?x; Close) -> dualof *!a 1-> ()
runTailNode next tail =
    let i = receive_ @a tail in
    let next' = 
        forkWith @dualof (rec x . ?a; ?x; Close) @()
            (\c:(rec x . ?a; ?x; Close) 1-> send i next |> send c |> wait) 
        in
    runTailNode @a next' tail 

{- queue -}     

initQueue : () -> (*?a, *!a)
initQueue _ =
    let (internalC, internalS) = new @(rec x . ?a; ?x; Close) () in
    ( forkWith @*?a @() (runHeadNode @a internalC)
    , forkWith @*!a @() (runTailNode @a internalS)
    )

enqueue : a -> (*?a, *!a) 1-> ()
enqueue i queue = 
    send_ @a i $ snd @*?a @*!a queue

dequeue : (*?a, *!a) -> a
dequeue queue = 
    receive_ @a $ fst @*?a @*!a queue

---------------------------------- SharedList ----------------------------------

{- list structure (while there's no native one) -}

data List = Nil 
          | Cons (ProductId, Issue, RmaNumber) List

{- channel types -}

type SharedList = *?ListC
type ListC = +{ Append: !ProductId; !Issue; !RmaNumber; ListC
              , CloseL: Close 
              }

{- list server -}

initList : SharedList
initList = forkWith @SharedList @() runListServer

runListServer : dualof SharedList 1-> ()
runListServer ch =
    runServer @ListC @List runListService Nil ch

runListService : List -> dualof ListC 1-> List
runListService list ch = 
    match ch with {
        Append ch -> 
            let (productId, ch) = receive ch in
            let (issue    , ch) = receive ch in
            let (rmaNumber, ch) = receive ch in
            -- logging
            receive_ @OutStream stdout
            |> hPutStr "RMA processed \t\t @ product id: "
            |> hPutStr (show @Int productId)
            |> hPutStr ", issue: "
            |> hPutStr issue
            |> hPutStr ", RMA id: "
            |> hPutStrLn (show @Int rmaNumber)
            |> hCloseOut;
            --
            runListService (Cons (productId, issue, rmaNumber) list) ch,
        CloseL c ->
            wait c; 
            list
    }

{- client functions -}

append : SharedList -> (ProductId, Issue, RmaNumber) -> ()
append ch triple =
    let (productId, pair) = triple in
    let (issue, rmaNumber) = pair in
    receive_ @ListC ch
    |> select Append
    |> send productId
    |> send issue
    |> send rmaNumber
    |> select CloseL
    |> close 

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

isNothing : MaybeValue -> Bool
isNothing maybeVal =
    case maybeVal of {
        JustValue _  -> False,
        NothingValue -> True
    }

fromJustOrDefault :  (Amount, Price) -> MaybeValue -> (Amount, Price)
fromJustOrDefault default maybeVal =
    case maybeVal of {
        JustValue val -> val,
        NothingValue -> default
    }

{- channel types -}

type SharedMap = *?MapC
type MapC = +{ Put: !ProductName; ValueC     ; MapC
             , Get: !ProductName; MaybeValueC; MapC
             , CloseM: Close
             }

type ValueC = !Amount; !Price

type MaybeValueC = &{ JustVal: dualof ValueC
                    , NothingVal: Skip
                    }

{- map server -}

initMapWith : Map -> SharedMap
initMapWith map = forkWith @SharedMap @() (runMapServer map)

runMapServer : Map -> dualof SharedMap 1-> ()
runMapServer map ch = 
    runServer @MapC @Map runMapService map ch

runMapService : Map -> dualof MapC 1-> Map
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
                            select JustVal ch |> 
                            send amount |>
                            send price
                     } in
            runMapService map ch,
        CloseM ch -> 
            wait ch; 
            map
    }

{- client functions -}

putLin : ProductName -> (Amount, Price) -> MapC 1-> MapC
putLin pName val ch = 
    let (amount, price) = val in
    select Put ch
    |> send pName
    |> send amount
    |> send price

getLin : ProductName -> MapC 1-> (MaybeValue, MapC)
getLin pName ch =
    let ch = select Get ch |> 
             send pName in
    match ch with {
        JustVal ch -> 
            let (amount, ch) = receive ch in
            let (price, ch)  = receive ch in
            (JustValue (amount, price), ch),
        NothingVal ch -> 
            (NothingValue, ch)
    }

---------------------------------- Bank ----------------------------------

{- channel types -}

type Bank = *?BankService
type BankService = {- CreatePayment: -} !Price; ?PaymentC; Close

type PaymentC = !CCNumber; !CCCode; Close 

{- bank worker -}


initBank : Bank
initBank = 
    -- runWith [Bank] $
    --     \bank:dualof Bank 1-> parallel 3 (bankWorker bank)
    let (c, s) = new @Bank () in
    parallel @() 2 (\_:() -> bankWorker s);
    c

bankWorker : dualof Bank -> ()
bankWorker =
    runServer @BankService @() (runBankService) ()

-- TODO: Expand this function
-- | Executes a function
runWith : forall a . (dualof a 1-> ()) -> a
runWith f =
    let (c, s) = new @a () in
    f s;
    c

runBankService : () -> dualof BankService 1-> ()
runBankService _ ch =
    let (price, ch) = receive ch in
    runWith @dualof PaymentC (\c:PaymentC 1-> send c ch |> wait)
    |> runPayment price
    

runPayment : Price -> dualof PaymentC -> ()
runPayment price ch =
    -- mock up 
    let (ccnumber, ch) = receive ch in
    let cccode = receiveAndWait @Int ch in 
    -- logging
    receive_ @OutStream stdout
    |> hPutStr "Payment processed \t @ amount: "
    |> hPutStr (show @Int price)
    |> hPutStr ", credit card: "
    |> hPutStr (show @Int ccnumber)
    |> hPutStr ", credit card code: "
    |> hPutStrLn (show @Int cccode)
    |> hCloseOut

{- client functions -}

createPayment : Price -> Bank -> PaymentC
createPayment price bank =
    let ch = receive_ @BankService bank in
    let (p, ch) = receive $ send price ch in
    close ch; p


---------------------------------- Tech Store ----------------------------------

type ProductId = Int
type Issue = String
type RmaNumber = Int

type CCNumber = Int
type CCCode = Int

{- channel types -}

type TechStore   = *?TechService
type TechService = +{ Buy: ?BuyC
                    , Rma: ?RmaC
                    } ; Close

type BuyC = !ProductName; AvailabilityC
type RmaC = !ProductId; !Issue; ?RmaNumber; Close 

type AvailabilityC = &{ Available : ?Price; CheckoutC
                      , OutOfStock: Close
                      }

type CheckoutC = +{ Confirm: ?PaymentC; Close 
                  , Cancel : Close
                  }


{- store front -}

type BuyQueue = (*?dualof BuyC, *!dualof BuyC)
type RmaQueue = (*?dualof RmaC, *!dualof RmaC)

runStoreFront : BuyQueue -> RmaQueue -> dualof TechStore 1-> ()
runStoreFront buyQueue rmaQueue store =
    match accept @TechService store with {
        Buy ch ->
            let (c, s) = new @BuyC () in
            send c ch |> wait;
            enqueue @dualof BuyC s buyQueue,
        Rma ch ->
            let (c, s) = new @RmaC () in
            send c ch |> wait;
            enqueue @dualof RmaC s rmaQueue
            -- enqueue [dualof RmaC] (fst [dualof RmaC, Skip] (accept [RmaC, Skip] ch)) rmaQueue
    };
    runStoreFront buyQueue rmaQueue store

{- buy workers -}

buyWorker : BuyQueue -> SharedMap -> Bank -> ()
buyWorker buyQueue map bank = 
    let ch = dequeue @dualof BuyC buyQueue in
    --
    let (pName, ch) = receive ch in
    -- (try to) reserve from stock
    let (amount, price) = fromJustOrDefault (0, 0) $ getFromStock pName map in
    if amount < 1
    then select OutOfStock ch |> wait 
    else
        let ch = select Available ch |> send price in
            match ch with {
                Confirm ch -> send (createPayment price bank) ch |> wait,
                Cancel ch  -> wait ch; returnToStock pName map
            }
    ;
    --
    buyWorker buyQueue map bank

getFromStock : ProductName -> SharedMap -> MaybeValue
getFromStock pName map =
    -- get map access
    let mapS = receive_ @MapC map in
    -- get from map
    let (maybeVal, mapS) = getLin pName mapS in
    -- 
    case maybeVal of {
        NothingValue -> mapS,
        JustValue val -> 
            let (amount, price) = val in
            if amount > 0
            -- if stock > 0, decrement stock (reserves it)
            then putLin pName (amount-1, price) mapS
            -- if no stock, nothing
            else mapS
    } |> select CloseM |> close ;
    --
    maybeVal
    

returnToStock : ProductName -> SharedMap -> ()
returnToStock pName map =
    -- get map access
    let mapS = receive_ @MapC map in
    -- get from map
    let (maybeVal, mapS) = getLin pName mapS in
    case maybeVal of {
        NothingValue -> mapS,
        JustValue val -> 
            let (amount, price) = val in
            putLin pName (amount+1, price) mapS
    } |> select CloseM |> close 

{- rma workers -}

rmaWorker : RmaQueue -> Counter -> SharedList -> ()
rmaWorker rmaQueue counter rmaList =
    let ch = dequeue @dualof RmaC rmaQueue in
    --
    let (productId, ch) = receive ch in
    let (issue    , ch) = receive ch in
    let rmaNumber = receive_ @Int counter in
    append rmaList (productId, issue, rmaNumber);
    send rmaNumber ch |> wait ;
    --
    rmaWorker rmaQueue counter rmaList

{- store setup -}

setupStore : Bank -> TechStore 
setupStore bank =
    -- buy
    let buyQueue = initQueue @dualof BuyC () in
    let stockMap = initMapWith initialStock in
    parallel @() 3 (\_:() -> buyWorker buyQueue stockMap bank);
    -- rma
    let rmaQueue = initQueue @dualof RmaC () in
    let counter = initCounter in
    let rmaList = initList in
    parallel @()  1 (\_:() -> rmaWorker rmaQueue counter rmaList);
    -- store front
    forkWith @TechStore @() $ runStoreFront buyQueue rmaQueue

initialStock : Map
initialStock = 
    mapPut 'C' (2, 20) $
    mapPut 'B' (3, 5 ) $
    mapPut 'A' (1, 50) $
    Empty


---------------------------------- Clients ----------------------------------

{- buy clients -}

client0 : TechStore -> ()
client0 ch = 
    -- wait to be served by store
    let store = receive_ @TechService ch in
    -- go to the buy queue
    let (buyC, c) = receive $ select Buy store in
    close c;
    -- wait & do my business
    -- ask for product 'A'
    let buyC = send 'A' buyC in
    match buyC with {
        OutOfStock c ->
            close c; 
            putStrLn "[Client 0] I was unable to buy product 'A'",
        Available buyC -> 
            let (price, buyC) = receive buyC in
            -- buyer's price limit
            if price > 100 
            then buyC |> select Cancel
                      |> close
            else buyC |> select Confirm 
                      |> receiveAndClose @(PaymentC;Close)
                      |> send 123123123
                      |> send 123 
                      |> close
    }

{- rma clients -}

client1 : TechStore -> ()
client1 ch =
    -- wait to be served by store
    let store = receive_ @TechService ch in
    -- go to the rma queue
    let (rmaC, c) = receive $ select Rma store in
    close c ;
    -- wait & do my business
    let rmaId = rmaC |> send 1234567890
                     |> send "Monitor flickers when punched"
                     |> receiveAndClose @Int in
    ()

---------------------------------- Main ----------------------------------

{- main -}

main : ()
main =
    let bank = initBank in
    let store = setupStore bank in
    fork (\_:() 1-> client0 store);
    fork (\_:() 1-> client0 store);
    fork (\_:() 1-> client1 store);
    diverge 

diverge : ()
diverge = diverge
