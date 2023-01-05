---------------------------------- SharedCounter ----------------------------------

{- channel types -}

type StdOut  : *S = *?Printer
type Printer : 1S = +{ PrintBool    : !Bool  ; Printer
                     , PrintBoolLn  : !Bool  ; Printer
                     , PrintInt     : !Int   ; Printer
                     , PrintIntLn   : !Int   ; Printer
                     , PrintChar    : !Char  ; Printer
                     , PrintCharLn  : !Char  ; Printer
                     , PrintString  : !String; Printer
                     , PrintStringLn: !String; Printer
                     , Close        : End
                     }

{- server -}

initStdout : StdOut
initStdout = forkWith @StdOut @() runStdout

-- Can't use eta-reduction here because we don't have subtyping.
-- The main reason is due to the closure of runServer that will be
-- '*!(dualof StdOut) -> ()' which is different from the type of this function:
-- 'dualof StdOut 1-> ()'. Lin vs Un functions.
runStdout  : dualof StdOut 1-> ()
runStdout c = runServer @Printer @() runPrinter () c

runPrinter : () -> dualof Printer 1-> ()
runPrinter _ printer =
    match printer with {
        PrintBool     printer -> aux @Bool   printer printBool     |> runPrinter (),
        PrintBoolLn   printer -> aux @Bool   printer printBoolLn   |> runPrinter (),
        PrintInt      printer -> aux @Int    printer printInt      |> runPrinter (),
        PrintIntLn    printer -> aux @Int    printer printIntLn    |> runPrinter (),
        PrintChar     printer -> aux @Char   printer printChar     |> runPrinter (),
        PrintCharLn   printer -> aux @Char   printer printCharLn   |> runPrinter (),
        PrintString   printer -> aux @String printer printString   |> runPrinter (),
        PrintStringLn printer -> aux @String printer printStringLn |> runPrinter (),
        Close         printer -> close printer
    }

aux : forall a . ?a;dualof Printer -> (a -> ()) 1-> dualof Printer
aux printer printFun =
    let (x, printer) = receive printer in
    printFun x;
    printer

{- client functions -}

printGenericLin : forall a . (Printer -> !a;Printer) -> a -> Printer -> Printer
printGenericLin sel x printer = 
    sel printer |> send x 

printGenericUn : forall a . (Printer -> !a;Printer) -> a -> StdOut -> ()
printGenericUn sel x stdout =
    printGenericLin @a sel x (receive_ @Printer stdout) |> 
    select Close |> close

printStringLin : String -> Printer -> Printer
printStringLin = printGenericLin @String (\printer:Printer -> select PrintString printer)

printStringLnLin : String -> Printer -> Printer
printStringLnLin = printGenericLin @String (\printer:Printer -> select PrintStringLn printer)

printIntLin : Int -> Printer -> Printer
printIntLin = printGenericLin @Int (\printer:Printer -> select PrintInt printer)

printIntLnLin : Int -> Printer -> Printer
printIntLnLin = printGenericLin @Int (\printer:Printer -> select PrintIntLn printer)

---------------------------------- SharedCounter ----------------------------------

type Counter : *S = *?Int

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

-- type Internal : 1S = ?Int; ?Internal

{- nodes -}

runHeadNode : forall a:1T . (rec x:1S . ?a; ?x; End) -> dualof *?a 1-> ()
runHeadNode prev head = 
    -- receive value |> next node endpoint
    let (i, prev) = receive prev in
    let (prev', prev) = receive prev in
    close prev;
    -- send value to client
    send_ @a i head;
    -- run node with new endpoint
    runHeadNode @a prev' head

runTailNode : forall a:1T . dualof (rec x:1S . ?a; ?x; End) -> dualof *!a 1-> ()
runTailNode next tail =
    let i = receive_ @a tail in
    let next' = 
        forkWith @dualof (rec x:1S . ?a; ?x; End) @()
            (\c:(rec x:1S . ?a; ?x; End) 1-> send i next |> send c |> close) 
        in
    runTailNode @a next' tail 

{- queue -}

initQueue : forall a:1T . () -> (*?a, *!a)
initQueue _ =
    let (internalC, internalS) = new @(rec x:1S . ?a; ?x; End) in
    ( forkWith @*?a @() (runHeadNode @a internalC)
    , forkWith @*!a @() (runTailNode @a internalS)
    )

enqueue : forall a:1T . a -> (*?a, *!a) 1-> ()
enqueue i queue = 
    send_ @a i $ snd @*?a @*!a queue

dequeue : forall a:1T . (*?a, *!a) -> a
dequeue queue = 
    receive_ @a $ fst @*?a @*!a queue

---------------------------------- SharedList ----------------------------------

{- list structure (while there's no native one) -}

data List = Nil 
          | Cons (ProductId, Issue, RmaNumber) List

{- channel types -}

type SharedList : *S = *?ListC
type ListC : 1S = +{ Append: !ProductId; !Issue; !RmaNumber; ListC
                   , Close: End 
                   }

{- list server -}

initList : StdOut -> SharedList
initList stdout = forkWith @SharedList @() (runListServer stdout)

runListServer : StdOut -> dualof SharedList 1-> ()
runListServer stdout ch =
    runServer @ListC @List (runListService stdout) Nil ch

runListService : StdOut -> List -> dualof ListC 1-> List
runListService stdout list ch = 
    match ch with {
        Append ch -> 
            let (productId, ch) = receive ch in
            let (issue    , ch) = receive ch in
            let (rmaNumber, ch) = receive ch in
            -- logging
            receive_ @Printer stdout |>
            printStringLin "RMA processed \t\t @ product id: " |>
            printIntLin    productId |>
            printStringLin ", issue: " |>
            printStringLin    issue |>
            printStringLin ", RMA id: " |>
            printIntLnLin  rmaNumber |>
            select Close |> close;
            --
            runListService stdout (Cons (productId, issue, rmaNumber) list) ch,
        Close c ->
            close c; 
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
    |> select Close
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

type SharedMap : *S = *?MapC
type MapC : 1S = +{ Put: !ProductName; ValueC     ; MapC
                  , Get: !ProductName; MaybeValueC; MapC
                  , Close: End
                  }

type ValueC : 1S = !Amount; !Price

type MaybeValueC : 1S = &{ JustVal: dualof ValueC
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
        Close ch -> 
            close ch; 
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

type Bank : *S = *?BankService
type BankService : 1S = {- CreatePayment: -} !Price; ?PaymentC; End

type PaymentC : 1S = !CCNumber; !CCCode; End 

{- bank worker -}


initBank : StdOut -> Bank
initBank stdout = 
    -- runWith [Bank] $
    --     \bank:dualof Bank 1-> parallel 3 (bankWorker bank)
    let (c, s) = new @Bank () in
    parallel @() 2 (\_:() -> bankWorker stdout s);
    c

bankWorker : StdOut -> dualof Bank -> ()
bankWorker stdout =
    runServer @BankService @() (runBankService stdout) ()

-- TODO: Expand this function
-- | Executes a function
runWith : forall a:1S . (dualof a 1-> ()) -> a
runWith f =
    let (c, s) = new @a () in
    f s;
    c

runBankService : StdOut -> () -> dualof BankService 1-> ()
runBankService stdout _ ch =
    let (price, ch) = receive ch in
    runWith @dualof PaymentC (\c:PaymentC 1-> send c ch |> close)
    |> runPayment stdout price
    

runPayment : StdOut -> Price -> dualof PaymentC -> ()
runPayment stdout price ch =
    -- mock up 
    let (ccnumber, ch) = receive ch in
    let cccode = receiveAndClose @Int ch in 
    -- logging
    receive_ @Printer stdout
    |> printStringLin "Payment processed \t @ amount: "
    |> printIntLin    price
    |> printStringLin ", credit card: "
    |> printIntLin    ccnumber
    |> printStringLin ", credit card code: "
    |> printIntLnLin  cccode
    |> select Close
    |> close


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

type TechStore   : *S = *?TechService
type TechService : 1S = +{ Buy: ?BuyC
                         , Rma: ?RmaC
                         }

type BuyC : 1S = !ProductName; AvailabilityC
type RmaC : 1S = !ProductId; !Issue; ?RmaNumber; End 

type AvailabilityC : 1S = &{ Available : ?Price; CheckoutC
                           , OutOfStock: End
                           }

type CheckoutC : 1S = +{ Confirm: ?PaymentC; End 
                       , Cancel : End
                       }


{- store front -}

type BuyQueue = (*?dualof BuyC, *!dualof BuyC)
type RmaQueue = (*?dualof RmaC, *!dualof RmaC)

runStoreFront : BuyQueue -> RmaQueue -> dualof TechStore 1-> ()
runStoreFront buyQueue rmaQueue store =
    match accept_ @TechService store with {
        Buy ch ->
            enqueue @dualof BuyC (accept @BuyC @Skip ch) buyQueue,
        Rma ch ->
            enqueue @dualof RmaC (accept @RmaC @Skip ch) rmaQueue
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
    then select OutOfStock ch |> close 
    else
        let ch = select Available ch |> send price in
            match ch with {
                Confirm ch -> send (createPayment price bank) ch |> close,
                Cancel ch  -> close ch; returnToStock pName map
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
    } |> select Close |> close ;
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
    } |> select Close |> close 

{- rma workers -}

rmaWorker : RmaQueue -> Counter -> SharedList -> ()
rmaWorker rmaQueue counter rmaList =
    let ch = dequeue @dualof RmaC rmaQueue in
    --
    let (productId, ch) = receive ch in
    let (issue    , ch) = receive ch in
    let rmaNumber = receive_ @Int counter in
    append rmaList (productId, issue, rmaNumber);
    send rmaNumber ch |> close ;
    --
    rmaWorker rmaQueue counter rmaList

{- store setup -}

setupStore : StdOut -> Bank -> TechStore 
setupStore stdout bank =
    -- buy
    let buyQueue = initQueue @dualof BuyC () in
    let stockMap = initMapWith initialStock in
    parallel @() 3 (\_:() -> buyWorker buyQueue stockMap bank);
    -- rma
    let rmaQueue = initQueue @dualof RmaC () in
    let counter = initCounter in
    let rmaList = initList stdout in
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

client0 : StdOut -> TechStore -> ()
client0 stdout ch = 
    -- wait to be served by store
    let store = receive_ @TechService ch in
    -- go to the buy queue
    let (buyC, _) = receive $ select Buy store in
    -- wait & do my business
    -- ask for product 'A'
    let buyC = send 'A' buyC in
    match buyC with {
        OutOfStock c ->
            close c; 
            printStringLnUn "[Client 0] I was unable to buy product 'A'" stdout,
        Available buyC -> 
            let (price, buyC) = receive buyC in
            -- buyer's price limit
            if price > 100 
            then buyC |> select Cancel
                      |> close
            else buyC |> select Confirm 
                      |> receiveAndClose @PaymentC;End 
                      |> send 123123123
                      |> send 123 
                      |> close
    }

{- rma clients -}

client1 : StdOut -> TechStore -> ()
client1 _ ch =
    -- wait to be served by store
    let store = receive_ @TechService ch in
    -- go to the rma queue
    let (rmaC, _) = receive $ select Rma store in
    -- wait & do my business
    let rmaId = rmaC |> send 1234567890
                     |> send "Monitor flickers when punched"
                     |> receiveAndClose @Int in
    ()

---------------------------------- Main ----------------------------------

{- main -}

main : ()
main =
    let stdout = initStdout in
    --
    let bank = initBank stdout in
    let store = setupStore stdout bank in
    fork (\_:() 1-> client0 stdout store);
    fork (\_:() 1-> client0 stdout store);
    fork (\_:() 1-> client1 stdout store);
    diverge 
