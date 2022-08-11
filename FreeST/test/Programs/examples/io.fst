main : ()
main =
    -- | use stdout print functions
    putStr (show @Bool True); putStrLn (show @Bool False);
    putStr (show @Char 'a') ; putStrLn (show @Char 'b');
    putStr (show @Int 1)    ; putStrLn (show @Int 2);
    putStr "Hello, "        ; putStrLn "World!";
    -- | print to stdout manually
    receive_ @OutStream stdout & hPutStrLn "------------------" & hCloseOut; 
    -- | print to stderr using put functions
    hPutStr_ "Error: " stderr; hPutStrLn_ "everything is fine!" stderr;
    -- | get values from stdin
    putStr "Insert a bool:";
    print @Bool   $ readBool $ getLine;
    putStr "Insert an int:";
    print @Int    $ readInt  $ getLine;
    putStr "Insert a char:";
    print @Char   $ readChar $ getLine;
    putStr "Insert a string:";
    print @String $            getLine;
    -- | write to file
    openWriteFile "teste.txt" &
    hPutStrLn "Hello, World!" &
    hCloseOut;
    -- | wait a bit ...
    getLine;
    -- | read from same file
    let (contents, ch) = hGetLine $ openReadFile "teste.txt" in
    hCloseIn ch;
    print @String contents;
    -- | extra print
    putStrLn "-end-";
    -- | unit!
    ()