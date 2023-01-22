module File where

-- | Open an `OutStream` channel endpoint to a file specified by a path, in write mode.
openWriteFile : FilePath -> OutStream
openWriteFile fp =
    forkWith @OutStream @() $ __runWriteFile (__openFile fp WriteMode)

-- | Open an `OutStream` channel endpoint to a file specified by a path, in append mode.
openAppendFile : FilePath -> OutStream
openAppendFile fp =
    forkWith @OutStream @() $ __runWriteFile (__openFile fp AppendMode)

__runWriteFile : FileHandle -> dualof OutStream 1-> ()
__runWriteFile fh ch =
    match ch with {
        PutChar  ch -> let (c, ch) = receive ch in __putFileStr fh (show @Char c); __runWriteFile fh ch,
        PutStr   ch -> let (s, ch) = receive ch in __putFileStr fh s             ; __runWriteFile fh ch,
        PutStrLn ch -> let (s, ch) = receive ch in __putFileStr fh (s ++ "\n")   ; __runWriteFile fh ch,
        Close    ch -> __closeFile fh; close ch
    }

-- | Open an InStream channel endpoint to a file specified by a path, in read mode.
openReadFile : FilePath -> InStream
openReadFile fp = 
    forkWith @InStream @() $ __runReadFile (__openFile fp ReadMode)

__runReadFile : FileHandle -> dualof InStream 1-> ()
__runReadFile fh ch =
    match ch with {
        GetChar ch -> __runReadFile fh $ send (__readFileChar fh) ch,
        GetLine ch -> __runReadFile fh $ send (__readFileLine fh) ch,
        IsEOF   ch -> __runReadFile fh $ send (__isEOF fh       ) ch,
        Close   ch -> __closeFile fh; close ch
    }

-- | Write a string to a file specified by a path. Does the same as `openWriteFile fp |> hPutStr s |> hCloseOut`.
writeFile : FilePath -> String -> ()
writeFile fp content = openWriteFile fp
                     |> hPutStr content
                     |> hCloseOut

-- | Write a string to a file specified by a path. Does the same as `openAppendFile fp |> hPutStr s |> hCloseOut`.
appendFile : FilePath -> String -> ()
appendFile fp content = openAppendFile fp
                     |> hPutStr content
                     |> hCloseOut

-- | Read the contents of a file specified by a path. Note that the string separates lines explicitely with the newline character `\n`.
readFile : FilePath -> String
readFile fp = 
    let (content, f) = openReadFile fp |> hGetContent in
    hCloseIn f;
    content