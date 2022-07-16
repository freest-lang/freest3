type StdOut  : *S = *?Printer
type Printer : 1S = +{ PrintBool    : !Bool  ; Printer
                     , PrintBoolLn  : !Bool  ; Printer
                     , PrintInt     : !Int   ; Printer
                     , PrintIntLn   : !Int   ; Printer
                     , PrintChar    : !Char  ; Printer
                     , PrintCharLn  : !Char  ; Printer
                     , PrintString  : !String; Printer
                     , PrintStringLn: !String; Printer
                     , Close        : Skip
                     }

-- helper functions

printGeneric : forall a . (Printer -> !a;Printer) -> StdOut -> a -> Skip
printGeneric sel stdout x =
    let (printer, _) = receive stdout in
    sel printer & send x & select Close

printBool' : StdOut -> Bool -> Skip
printBool' = printGeneric @Bool (\printer:Printer -> select PrintBool printer) 

printBoolLn' : StdOut -> Bool -> Skip
printBoolLn' = printGeneric @Bool (\printer:Printer -> select PrintBoolLn printer)


printInt' : StdOut -> Int -> Skip
printInt' = printGeneric @Int (\printer:Printer -> select PrintInt printer)

printIntLn' : StdOut -> Int -> Skip
printIntLn' = printGeneric @Int (\printer:Printer -> select PrintIntLn printer)


printChar' : StdOut -> Char -> Skip
printChar' = printGeneric @Char (\printer:Printer -> select PrintChar printer)

printCharLn' : StdOut -> Char -> Skip
printCharLn' = printGeneric @Char (\printer:Printer -> select PrintCharLn printer)


printString' : StdOut -> String -> Skip
printString' = printGeneric @String (\printer:Printer -> select PrintString printer)

printStringLn' : StdOut -> String -> Skip
printStringLn' = printGeneric @String (\printer:Printer -> select PrintStringLn printer)

-- stdout server

runStdout  : dualof StdOut -> ()
runStdout =
    runServer @Printer @() runPrinter ()

runPrinter : () -> dualof Printer 1-> ()
runPrinter _ printer =
    match printer with {
        PrintBool     printer -> aux @Bool   printer printBool     & runPrinter (),
        PrintBoolLn   printer -> aux @Bool   printer printBoolLn   & runPrinter (),
        PrintInt      printer -> aux @Int    printer printInt      & runPrinter (),
        PrintIntLn    printer -> aux @Int    printer printIntLn    & runPrinter (),
        PrintChar     printer -> aux @Char   printer printChar     & runPrinter (),
        PrintCharLn   printer -> aux @Char   printer printCharLn   & runPrinter (),
        PrintString   printer -> aux @String printer printString   & runPrinter (),
        PrintStringLn printer -> aux @String printer printStringLn & runPrinter (),
        Close         _       -> ()
    }

aux : forall a . ?a;dualof Printer -> (a -> ()) 1-> dualof Printer
aux printer printFun =
    let (x, printer) = receive printer in
    printFun x;
    printer


-- selfish client

-- a client that captures stdout to print two strings uninterruptedly
client : StdOut -> String -> String -> ()
client stdout s1 s2 =
    let printer = fst @Printer @StdOut $ receive stdout in
    let _ = 
        select PrintString printer & send s1 &
        select PrintString         & send s2 &
        select Close
        in ()

-- main

main : ()
main = 
    let (stdout, s) = new StdOut in
    fork $ client stdout "A" "B";
    fork $ client stdout "C" "D";
    runStdout s -- run stdout server




