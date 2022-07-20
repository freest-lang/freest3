main : ()
main =
    -- use stdout print functions
    printBool     True;
    printBoolLn   False;
    printChar     'a';
    printCharLn   'b';
    printInt      1;
    printIntLn    2;
    printString   "Hello, ";
    printStringLn "World!";
    -- print to stdout manually
    receive_ @OutStream stdout & select PutStringLn & send "------------------" & select Close & (sink @Skip); 
    -- print to stderr using put functions
    
    -- get values from stdin
    case inputBool   of { JustBool   b -> printBoolLn   b, NothingBool   -> printStringLn "oooops"};
    case inputInt    of { JustInt    i -> printIntLn    i, NothingInt    -> printStringLn "oooops"};
    case inputChar   of { JustChar   c -> printCharLn   c, NothingChar   -> printStringLn "oooops"};
    case inputString of { JustString s -> printStringLn s, NothingString -> printStringLn "oooops"};
    -- unit!
    printStringLn "";
    ()