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

-- helper functions

printGeneric : forall a . StdOut -> a -> (Printer -> !a;Printer) -> Skip
printGeneric stdout x sel =
    let (printer, _) = receive stdout in
    sel printer & send x & select Close

printBool' : Bool -> StdOut -> Skip
printBool'   b stdout = printGeneric[Bool] stdout b (\printer:select PrintBool printer) 

printBoolLn' : Bool -> StdOut -> Skip
printBoolLn' b stdout = printGeneric[Bool] stdout b (\printer:Printer -> select PrintBoolLn printer)


printInt' : Int -> StdOut -> Skip
printInt' i stdout = printGeneric[Int] stdout i (\printer:Printer -> select PrintInt printer)

printIntLn' : Int -> StdOut -> Skip
printIntLn' i stdout = printGeneric[Int] stdout i (\printer:Printer -> select PrintIntLn printer)


printChar' : Char -> StdOut -> Skip
printChar' c stdout = printGeneric[Char] stdout c (\printer:Printer -> select PrintChar printer)

printCharLn' : Char -> StdOut -> Skip
printCharLn' c stdout = printGeneric[Char] stdout c (\printer:Printer -> select PrintCharLn printer)


printString' : String -> StdOut -> Skip
printString' s stdout = printGeneric[String] stdout s (\printer:Printer -> select PrintString printer)

printStringLn' : String -> StdOut -> Skip
printStringLn' s stdout = printGeneric[String] stdout s (\printer:Printer -> select PrintStringLn printer)

-- stdout server

runStdout  : dualof StdOut -> ()
runStdout stdout =
    let (c, s) = new Printer in
    let stdout = send c stdout in
    runPrinter s;
    runStdout stdout 

runPrinter : dualof Printer -> ()
runPrinter printer =
    match printer with {
        PrintBool     printer -> aux[Bool]   printer printBool     & runPrinter,
        PrintBoolLn   printer -> aux[Bool]   printer printBoolLn   & runPrinter,
        PrintInt      printer -> aux[Int]    printer printInt      & runPrinter,
        PrintIntLn    printer -> aux[Int]    printer printIntLn    & runPrinter,
        PrintChar     printer -> aux[Char]   printer printChar     & runPrinter,
        PrintCharLn   printer -> aux[Char]   printer printCharLn   & runPrinter,
        PrintString   printer -> aux[String] printer printString   & runPrinter,
        PrintStringLn printer -> aux[String] printer printStringLn & runPrinter,
        Close         _       -> ()
    }

aux : forall a . ?a;dualof Printer -> (a -> ()) -o dualof Printer
aux printer printFun =
    let (x, printer) = receive printer in
    printFun x;
    printer



main : Skip
main = 
    let (stdout, s) = new StdOut in
    fork $ runStdout s;
    printStringLn' "Hello, World!" stdout




