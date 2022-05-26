---------------------------------- Util.fst ----------------------------------
-- module Util where

sink : forall a . a -> ()
sink _ = ()

---------------------------------- Concurrent.fst ----------------------------------
-- module Concurrent where

-- type Handler a = dualof a -o ()

-- | Receive a value from a shared channel
receive_ : forall a:TL . *?a -> a
receive_ ch = fst [a, *?a] $ receive ch

-- | Send a value on a shared channel
send_ : forall a:TL . a -> *!a -o ()
send_ x ch = sink [*!a] $ send x ch

-- | Executes a function
runWith : forall a:SL . (dualof a -o ()) -> a
runWith f =
    let (c, s) = new a in
    f s;
    c

-- | Execute a thunk n times sequentially
repeat : forall a . Int -> (() -> a) -> ()
repeat n thunk =
    if n < 0
    then ()
    else 
        thunk ();
        repeat [a] (n - 1) thunk

-- | Fork a thunk n times
parallel : Int -> (() -> ()) -> ()
parallel n thunk = repeat [()] n (\_:() -> fork (thunk ()))

-- | Create a new child process and a linear channel through which it can 
--   communicate with its parent process.
forkWith : forall a:SL . (dualof a -o ()) -> a
forkWith f = --runWith[a] $ \c:dualof a -> fork (f c)
    let (c, s) = new a in
    fork $ f s;
    c

accept : forall a:SL b:SL . !a; b -> (dualof a, b)
accept ch =
    let (c, s) = new a in
    let ch = send c ch in
    (s, ch)

acceptUn : forall a:SL . *!a -> dualof a
acceptUn ch =
    fst [dualof a, *!a] $ accept [a, *!a] ch

runServer : forall a:SL b . *!a -> (b -> dualof a -o b) -> b -> ()
runServer ch handle state =
    runServer [a, b] ch handle $ handle state $ acceptUn [a] ch 

---------------------------------- SharedCounter ----------------------------------

{- channel types -}

type StdOut  : SU = *?Printer
type Printer : SL = +{ PrintBool    : !Bool  ; Printer
                     , PrintBoolLn  : !Bool  ; Printer
                     , PrintInt     : !Int   ; Printer
                     , PrintIntLn   : !Int   ; Printer
                     , PrintChar    : !Char  ; Printer
                     , PrintCharLn  : !Char  ; Printer
                     , PrintString  : !String; Printer
                     , PrintStringLn: !String; Printer
                     , Close        : Skip
                     }

{- server -}

initStdout : StdOut
initStdout = forkWith [StdOut] runStdout

runStdout  : dualof StdOut -o ()
runStdout stdout =
    runServer [Printer, ()] stdout runPrinter ()

runPrinter : () -> dualof Printer -o ()
runPrinter _ printer =
    match printer with {
        PrintBool     printer -> aux [Bool]   printer printBool     & runPrinter (),
        PrintBoolLn   printer -> aux [Bool]   printer printBoolLn   & runPrinter (),
        PrintInt      printer -> aux [Int]    printer printInt      & runPrinter (),
        PrintIntLn    printer -> aux [Int]    printer printIntLn    & runPrinter (),
        PrintChar     printer -> aux [Char]   printer printChar     & runPrinter (),
        PrintCharLn   printer -> aux [Char]   printer printCharLn   & runPrinter (),
        PrintString   printer -> aux [String] printer printString   & runPrinter (),
        PrintStringLn printer -> aux [String] printer printStringLn & runPrinter (),
        Close         _       -> ()
    }

aux : forall a . ?a;dualof Printer -> (a -> ()) -o dualof Printer
aux printer printFun =
    let (x, printer) = receive printer in
    printFun x;
    printer

{- client functions -}

printGenericLin : forall a . (Printer -> !a;Printer) -> a -> Printer -> Printer
printGenericLin sel x printer = 
    sel printer & send x 

printGenericUn : forall a . (Printer -> !a;Printer) -> a -> StdOut -> ()
printGenericUn sel x stdout =
    printGenericLin [a] sel x (receive_ [Printer] stdout) & 
    select Close &
    sink [Skip]

printStringLin : String -> Printer -> Printer
printStringLin = printGenericLin [String] (\printer:Printer -> select PrintString printer)

printStringLnLin : String -> Printer -> Printer
printStringLnLin = printGenericLin [String] (\printer:Printer -> select PrintStringLn printer)

printStringLnUn : String -> StdOut -> ()
printStringLnUn = printGenericUn [String] (\printer:Printer -> select PrintStringLn printer)

printIntLin : Int -> Printer -> Printer
printIntLin = printGenericLin [Int] (\printer:Printer -> select PrintInt printer)

printIntLnLin : Int -> Printer -> Printer
printIntLnLin = printGenericLin [Int] (\printer:Printer -> select PrintIntLn printer)

---------------------------------- SharedCounter ----------------------------------

type Counter : SU = *?Int

initCounter : Counter
initCounter = 
    forkWith [Counter] (\ch:*!Int -o runCounter 0 ch)

runCounter : Int -> dualof Counter -> ()
runCounter i counter =
    send_ [Int] i counter;
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
    send_ [a] i head;
    -- run node with new endpoint
    runHeadNode [a] prev head

runTailNode : forall a:TL . dualof (rec x:SL . ?a; ?x) -> dualof *!a -o ()
runTailNode next tail =
    let i = receive_ [a] tail in
    let next' = 
        forkWith [dualof (rec x:SL . ?a; ?x)] 
            (\c:(rec x:SL . ?a; ?x) -o send i next & send c & sink [Skip]) 
        in
    runTailNode [a] next' tail 

{- queue -}

initQueue : forall a:TL . () -> (*?a, *!a)
initQueue _ =
    let (internalC, internalS) = new (rec x:SL . ?a; ?x) in
    ( forkWith [*?a] (runHeadNode [a] internalC)
    , forkWith [*!a] (runTailNode [a] internalS)
    )

enqueue : forall a:TL . a -> (*?a, *!a) -o ()
enqueue i queue = 
    send_ [a] i $ snd [*?a, *!a] queue

dequeue : forall a:TL . (*?a, *!a) -> a
dequeue queue = 
    receive_ [a] $ fst [*?a, *!a] queue

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

initList : StdOut -> SharedList
initList stdout = forkWith [SharedList] (runListServer stdout)

runListServer : StdOut -> dualof SharedList -o ()
runListServer stdout ch =
    runServer [ListC, List] ch (runListService stdout) Nil 

runListService : StdOut -> List -> dualof ListC -o List
runListService stdout list ch = 
    match ch with {
        Append ch -> 
            let (productId, ch) = receive ch in
            let (issue    , ch) = receive ch in
            let (rmaNumber, ch) = receive ch in
            -- logging
            receive_ [Printer] stdout &
            printStringLin "RMA processed \t\t @ product id: " &
            printIntLin    productId &
            printStringLin ", issue: " &
            printStringLin    issue &
            printStringLin ", RMA id: " &
            printIntLnLin  rmaNumber &
            select Close &
            sink [Skip];
            --
            runListService stdout (Cons (productId, issue, rmaNumber) list) ch,
        Close _ -> 
            list
    }

{- client functions -}

append : SharedList -> (ProductId, Issue, RmaNumber) -> ()
append ch triple =
    let (productId, pair) = triple in
    let (issue, rmaNumber) = pair in
    receive_ [ListC] ch &
    select Append &
    send productId &
    send issue &
    send rmaNumber &
    select Close &
    sink [Skip]

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

type SharedMap : SU = *?MapC
type MapC : SL = +{ Put: !ProductName; ValueC     ; MapC
                  , Get: !ProductName; MaybeValueC; MapC
                  , Close: Skip
                  }

type ValueC : SL = !Amount; !Price

type MaybeValueC : SL = &{ JustVal: dualof ValueC
                         , NothingVal: Skip
                         }

{- map server -}

initMapWith : Map -> SharedMap
initMapWith map = forkWith [SharedMap] (runMapServer map)

runMapServer : Map -> dualof SharedMap -o ()
runMapServer map ch = 
    runServer [MapC, Map] ch runMapService map

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
        Close _ -> map
    }

{- client functions -}

putLin : ProductName -> (Amount, Price) -> MapC -o MapC
putLin pName val ch = 
    let (amount, price) = val in
    select Put ch &
    send pName &
    send amount &
    send price

getLin : ProductName -> MapC -o (MaybeValue, MapC)
getLin pName ch =
    let ch = select Get ch &
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

type Bank : SU = *?BankService
type BankService : SL = {- CreatePayment: -} !Price; ?PaymentC

type PaymentC : SL = !CCNumber; !CCCode

{- bank worker -}


initBank : StdOut -> Bank
initBank stdout = 
    -- runWith [Bank] $
    --     \bank:dualof Bank -o parallel 3 (bankWorker bank)
    let (c, s) = new Bank in
    parallel 2 (\_:() -> bankWorker stdout s);
    c

bankWorker : StdOut -> dualof Bank -> ()
bankWorker stdout ch =
    runServer [BankService, ()] ch (runBankService stdout) ()

runBankService : StdOut -> () -> dualof BankService -o ()
runBankService stdout _ ch =
    let (price, ch) = receive ch in
    runWith [dualof PaymentC] (\c:PaymentC -o send c ch & sink [Skip]) &
    runPayment stdout price
    

runPayment : StdOut -> Price -> dualof PaymentC -> ()
runPayment stdout price ch =
    -- mock up 
    let (ccnumber, ch) = receive ch in
    let (cccode, ch) = receive ch in
    -- logging
    receive_ [Printer] stdout &
    printStringLin "Payment processed \t @ amount: " &
    printIntLin    price &
    printStringLin ", credit card: " &
    printIntLin    ccnumber &
    printStringLin ", credit card code: " &
    printIntLnLin  cccode &
    select Close &
    sink [Skip]


{- client functions -}

createPayment : Price -> Bank -> PaymentC
createPayment price bank =
    let ch = receive_ [BankService] bank in
    fst [PaymentC, Skip] $ receive $ send price ch


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

type CheckoutC : SL = +{ Confirm: ?PaymentC
                       , Cancel : Skip
                       }


{- store front -}

type BuyQueue = (*?dualof BuyC, *!dualof BuyC)
type RmaQueue = (*?dualof RmaC, *!dualof RmaC)

runStoreFront : BuyQueue -> RmaQueue -> dualof TechStore -o ()
runStoreFront buyQueue rmaQueue store =
    match acceptUn[TechService] store with {
        Buy ch ->
            enqueue [dualof BuyC] (fst [dualof BuyC, Skip] (accept [BuyC, Skip] ch)) buyQueue,
        Rma ch ->
            enqueue [dualof RmaC] (fst [dualof RmaC, Skip] (accept [RmaC, Skip] ch)) rmaQueue
    };
    runStoreFront buyQueue rmaQueue store

{- buy workers -}

buyWorker : BuyQueue -> SharedMap -> Bank -> ()
buyWorker buyQueue map bank = 
    let ch = dequeue [dualof BuyC] buyQueue in
    --
    let (pName, ch) = receive ch in
    -- (try to) reserve from stock
    let (amount, price) = fromJustOrDefault (0, 0) $ getFromStock pName map in
    if amount < 1
    then select OutOfStock ch & sink [Skip]
    else
        let ch = select Available ch & send price in
            match ch with {
                Confirm ch -> send (createPayment price bank) ch,
                Cancel ch  -> returnToStock pName map; ch
            }
    ;
    --
    buyWorker buyQueue map bank

getFromStock : ProductName -> SharedMap -> MaybeValue
getFromStock pName map =
    -- get map access
    let mapS = receive_ [MapC] map in
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
    } &
    select Close &
    sink [Skip];
    --
    maybeVal
    

returnToStock : ProductName -> SharedMap -> ()
returnToStock pName map =
    -- get map access
    let mapS = receive_ [MapC] map in
    -- get from map
    let (maybeVal, mapS) = getLin pName mapS in
    case maybeVal of {
        NothingValue -> mapS,
        JustValue val -> 
            let (amount, price) = val in
            putLin pName (amount+1, price) mapS
    } &
    select Close &
    sink [Skip]

{- rma workers -}

rmaWorker : RmaQueue -> Counter -> SharedList -> ()
rmaWorker rmaQueue counter rmaList =
    let ch = dequeue [dualof RmaC] rmaQueue in
    --
    let (productId, ch) = receive ch in
    let (issue    , ch) = receive ch in
    let rmaNumber = receive_ [Int] counter in
    append rmaList (productId, issue, rmaNumber);
    send rmaNumber ch;
    --
    rmaWorker rmaQueue counter rmaList

{- store setup -}

setupStore : StdOut -> Bank -> TechStore 
setupStore stdout bank =
    -- buy
    let buyQueue = initQueue [dualof BuyC] () in
    let stockMap = initMapWith initialStock in
    parallel 3 (\_:() -> buyWorker buyQueue stockMap bank);
    -- rma
    let rmaQueue = initQueue [dualof RmaC] () in
    let counter = initCounter in
    let rmaList = initList stdout in
    parallel  1 (\_:() -> rmaWorker rmaQueue counter rmaList);
    -- store front
    forkWith [TechStore] $ runStoreFront buyQueue rmaQueue

initialStock : Map
initialStock = 
    mapPut 'C' (2, 20) $
    mapPut 'B' (3, 5 ) $
    mapPut 'A' (1, 50) $
    Empty


---------------------------------- Clients ----------------------------------

{- buy clients -}

client0 : StdOut -> TechStore -> ()
client0 stdout ch = 
    -- wait to be served by store
    let store = receive_ [TechService] ch in
    -- go to the buy queue
    let (buyC, _) = receive $ select Buy store in
    -- wait & do my business
    -- ask for product 'A'
    let buyC = send 'A' buyC in
    match buyC with {
        OutOfStock _ -> 
            printStringLnUn "[Client 0] I was unable to buy product 'A'" stdout,
        Available buyC -> 
            let (price, buyC) = receive buyC in
            -- buyer's price limit
            if price > 100 
            then  select Cancel buyC & sink [Skip]
            else
                let (paymentC, _) = select Confirm buyC & receive in
                send 123123123 paymentC & send 123 & sink [Skip]
    }

{- rma clients -}

client1 : StdOut -> TechStore -> ()
client1 _ ch =
    -- wait to be served by store
    let store = receive_ [TechService] ch in
    -- go to the rma queue
    let (rmaC, _) = receive $ select Rma store in
    -- wait & do my business
    let (rmaId, _) = send 1234567890 rmaC &
                     send "Monitor flickers when punched" &
                     receive in
    ()

---------------------------------- Main ----------------------------------

{- main -}

main : ()
main =
    let stdout = initStdout in
    --
    let bank = initBank stdout in
    let store = setupStore stdout bank in
    fork $ client0 stdout store;
    fork $ client0 stdout store;
    fork $ client1 stdout store;
    diverge ()

diverge : () -> ()
diverge = diverge
