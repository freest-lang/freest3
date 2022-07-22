main : ()
main =
    -- | use stdout print functions
    printBool     True;
    printBoolLn   False;
    printChar     'a';
    printCharLn   'b';
    printInt      1;
    printIntLn    2;
    printString   "Hello, ";
    printStringLn "World!";
    -- | print to stdout manually
    receive_ @OutStream stdout & hPutStringLn "------------------" & select Close; 
    -- | print to stderr using put functions
    putString "Error: " stderr; putStringLn "everything is fine!" stderr;
    -- | get values from stdin
    -- case inputBool   of { JustBool   b -> printBoolLn   b, NothingBool   -> printStringLn "oooops"};
    -- case inputInt    of { JustInt    i -> printIntLn    i, NothingInt    -> printStringLn "oooops"};
    -- case inputChar   of { JustChar   c -> printCharLn   c, NothingChar   -> printStringLn "oooops"};
    -- case inputString of { JustString s -> printStringLn s, NothingString -> printStringLn "oooops"};
    -- | write to file
    case openWriteFile "teste.txt" of {
        JustS ch -> 
            match hFilePutStringLn "Hello, World!" ch with {
                Ok ch -> printStringLn "Ok!"; select Close ch & sink @Skip,
                Error ch -> printStringLn "Error!"
            },
        NothingS -> ()
    };
    -- | extra print
    printStringLn "-end-";
    -- | unit!
    ()