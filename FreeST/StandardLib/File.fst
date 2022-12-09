module File where

openWriteFile : FilePath -> OutStream
openWriteFile fp =
    forkWith @OutStream @() $ __runWriteFile (__openFile fp WriteMode)

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

writeFile : FilePath -> String -> ()
writeFile fp content = openWriteFile fp
                     |> hPutStr content
                     |> hCloseOut

appendFile : FilePath -> String -> ()
appendFile fp content = openAppendFile fp
                     |> hPutStr content
                     |> hCloseOut

readFile : FilePath -> String
readFile fp = getContents $ openReadFile fp