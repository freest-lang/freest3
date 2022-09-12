{-

Prelude structure:
    1. Builtin        - builtin function definitions
    2. Prelude        - utility functions
    3. Concurrency    - functions for concurrency and shared channels
    4, Out/In Streams - input and output channels + util functions
    5. StdIO          - stdout, stdin, stderr
    6. Files          - open files for reading, writing and appending

(ASCII titles generated at https://ascii.today/)
-}

module Prelude where



--   $$\   
-- $$$$ |  
-- \_$$ |  
--   $$ |  
--   $$ |  
--   $$ |  
-- $$$$$$\ 
-- \______|



-- | Builtins

  -- Int
(+) : Int -> Int -> Int
(-) : Int -> Int -> Int
(*) : Int -> Int -> Int
(/) : Int -> Int -> Int
(^) : Int -> Int -> Int
mod : Int -> Int -> Int
rem : Int -> Int -> Int
div : Int -> Int -> Int
max : Int -> Int -> Int
min : Int -> Int -> Int
quot : Int -> Int -> Int
gcd : Int -> Int -> Int
lcm : Int -> Int -> Int
subtract : Int -> Int -> Int
succ : Int -> Int
pred : Int -> Int
abs : Int -> Int
negate : Int -> Int
even : Int -> Bool
odd : Int -> Bool
(==) : Int -> Int -> Bool
(/=) : Int -> Int -> Bool
(<) : Int -> Int -> Bool
(>) : Int -> Int -> Bool
(<=) : Int -> Int -> Bool
(>=) : Int -> Int -> Bool
  -- Bool
not : Bool -> Bool
(&&) : Bool -> Bool -> Bool
(||) : Bool -> Bool -> Bool
  -- Char
ord : Char -> Int
chr : Int -> Char
  -- String
(++) : String -> String -> String
show : ∀ a . a -> String
-- read : ∀ a . String -> a
readBool : String -> Bool
readInt : String -> Int
readChar : String -> Char
  -- Pair
fst : ∀ a:1T . ∀ b:*T . (a, b) -> a
snd : ∀ a:*T . ∀ b:1T . (a, b) -> b
  -- Internal Prints
__putStrOut : String -> ()
__putStrErr : String -> ()
  -- Internal Gets
__getChar : () -> Char
__getLine : () -> String
__getContents : () -> String
  -- Fork
fork : ∀a:1T. a -> ()
  -- Error & Undefined
error : ∀a:*T . String -> a
undefined : ∀a:*T . a
  -- Session ops
send : ∀a:1T . a -> ∀b:1S . !a;b 1-> b
receive : ∀a:1T . ∀b:1S . ?a;b -> (a, b)
  -- Not the actual type for collect, but for writing it we would
  -- need polymorphism over the labels in some choice/variant
collect : ∀a:1T . a
  -- Internal Files
__openFile : FilePath -> IOMode -> FileHandle
__putFileStr : FileHandle -> String -> ()
__readFileChar : FileHandle -> Char
__readFileLine : FileHandle -> String
__isEOF : FileHandle -> Bool
__closeFile : FileHandle -> ()



--  $$$$$$\  
-- $$  __$$\ 
-- \__/  $$ |
--  $$$$$$  |
-- $$  ____/ 
-- $$ |      
-- $$$$$$$$\ 
-- \________|

-- | Prelude

id : forall a . a -> a
id = \\a => \x:a -> x

flip : forall a b c . (a -> b -> c) -> b -> a -> c
flip f x y = f y x

until : forall a . (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until @a p f (f x)

-- | 'curry' converts an uncurried function to a curried function.
-- curry fst 1 2
-- 1

curry : forall a b c . ((a, b) -> c) -> a -> b -> c
curry f x y =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
-- uncurry (+) (1,2)
-- 3

uncurry  : forall a b c . (a -> b -> c) -> ((a, b) -> c)
uncurry f p =  f (fst@a @b p) (snd @a @b p)

-- | Swap the components of a pair.
swap : forall a b . (a,b) -> (b,a)
swap x = let (a,b) = x in (b,a)

-- |  Fixed-point Z combinator
fix : forall a . ((a -> a) -> (a -> a)) -> (a -> a)
fix f =
  (\x:(rec b.b -> (a -> a)) -> f (\z:a -> x x z))
  (\x:(rec b.b -> (a -> a)) -> f (\z:a -> x x z))



--  $$$$$$\  
-- $$ ___$$\ 
-- \_/   $$ |
--   $$$$$ / 
--   \___$$\ 
-- $$\   $$ |
-- \$$$$$$  |
--  \______/ 

-- | Concurrency

-- | A mark for functions that do not terminate
type Diverge = ()

-- | A function that diverges
diverge : () --Diverge
diverge = diverge

-- | Discard an unrestricted value
sink : forall a . a -> ()
sink _ = ()

-- | Execute a thunk n times, sequentially
repeat : forall a . Int -> (() -> a) -> ()
repeat n thunk =
    if n < 0
    then ()
    else 
        thunk ();
        repeat @a (n - 1) thunk

-- type Consumer a = a 1-> ()

-- | Receive a value from a linear channel and apply a function to it.
--   Returns the continuation channel
consume : forall a b:1S . (a -> ()) {- Consumer a -} -> ?a;b 1-> b
consume f ch =
    let (x, ch) = receive ch in
    f x;
    ch

-- | Receive a value from a star channel
receive_ : forall a:1T . *?a -> a
receive_ ch = fst @a @*?a $ receive ch

-- | Send a value on a star channel
send_ : forall a:1T . a -> *!a 1-> ()
send_ x ch = sink @*!a $ send x ch

-- | Fork n identical threads
parallel : Int -> (() -> ()) -> ()
parallel n thunk = repeat @() n (\_:() -> fork (thunk ()))

-- | Create a new child process and a linear channel through which it can 
--   communicate with its parent process. Return the channel endpoint.
forkWith : forall a:1S . (dualof a 1-> ()) -> a
forkWith f =
    let (x, y) = new a in
    fork $ f y;
    x

-- |Session initiation
-- |Accept a request for a linear session on a shared channel.
-- |The requester uses a conventional receive to obtain the channel end
accept : forall a:1S b:*S . !a; b -> dualof a
accept ch =
    let (x, y) = new a in
    send x ch;
    y

-- |Session initiation on an start channel
accept_ : forall a:1S . *!a -> dualof a
accept_ ch = accept @a @*!a ch

-- | Run a server process given its endpoint, a function to serve a client (a
-- handle) and the initial state.
runServer : forall a:1S b . (b -> dualof a 1-> b) -> b -> *!a -> () --Diverge
runServer handle state ch =
    runServer @a @b handle (handle state (accept_ @a ch)) ch 



-- $$\   $$\ 
-- $$ |  $$ |
-- $$ |  $$ |
-- $$$$$$$$ |
-- \_____$$ |
--       $$ |
--       $$ |
--       \__|

-- | Out/In Streams


-- | OutStream

type OutStreamProvider : *S = *?OutStream
type OutStream : 1S = +{ PutChar : !Char; OutStream
                       , PutStr  : !String; OutStream
                       , PutStrLn: !String; OutStream
                       , Close   : Skip
                       }

hCloseOut : OutStream -> ()
hCloseOut ch = sink @Skip $ select Close ch


hGenericPut : forall a . (OutStream -> !a;OutStream) -> a -> OutStream -> OutStream
hGenericPut sel x outStream = sel outStream & send x

hPutChar : Char -> OutStream -> OutStream
hPutChar = hGenericPut @Char (\ch:OutStream -> select PutChar ch) -- (select PutChar)

hPutStr, hPutStrLn : String -> OutStream -> OutStream
hPutStr   = hGenericPut @String (\ch:OutStream -> select PutStr   ch) -- (select PutStr)
hPutStrLn = hGenericPut @String (\ch:OutStream -> select PutStrLn ch) -- (select PutStrLn)

hPrint : forall a . a -> OutStream -> OutStream
hPrint x = hPutStrLn (show @a x)

hGenericPut_ : forall a . (a -> OutStream -> OutStream) -> a -> OutStreamProvider -> ()
hGenericPut_ putF x outProv = 
    hCloseOut $ putF x $ receive_ @OutStream outProv 

hPutChar_ : Char -> OutStreamProvider -> ()
hPutChar_ = hGenericPut_ @Char hPutChar

hPutStr_, hPutStrLn_ : String -> OutStreamProvider -> ()
hPutStr_   = hGenericPut_ @String hPutStr
hPutStrLn_ = hGenericPut_ @String hPutStrLn

hPrint_ : forall b . b -> OutStreamProvider -> ()
-- hPrint_ = hGenericPut_ @b (hPrint @b)
hPrint_ x ch = hGenericPut_ @b (hPrint @b) x ch


-- | InStream

type InStreamProvider : *S = *?InStream
type InStream : 1S = +{ GetChar     : ?Char  ; InStream
                      , GetLine     : ?String; InStream
                      , IsEOF       : ?Bool  ; InStream
                      , Close       : Skip
                      }

hCloseIn : InStream -> ()
hCloseIn ch = sink @Skip $ select Close ch

hGenericGet : forall a . (InStream -> ?a;InStream) -> InStream -> (a, InStream)
hGenericGet sel ch = receive $ sel ch

hGetChar : InStream -> (Char, InStream)
hGetChar = hGenericGet @Char (\ch:InStream -> select GetChar ch)

hGetLine : InStream -> (String, InStream)
hGetLine = hGenericGet @String (\ch:InStream -> select GetLine ch)

hIsEOF : InStream -> (Bool, InStream)
hIsEOF = hGenericGet @Bool (\ch:InStream -> select IsEOF ch)

hGenericGet_ : forall a . (InStream -> (a, InStream)) -> InStreamProvider -> a
hGenericGet_ getF inp = 
  let (x, ch) = getF $ receive_ @InStream inp in
  let _ = hCloseIn ch in x

hGetChar_ : InStreamProvider -> Char
hGetChar_ = hGenericGet_ @Char hGetChar

hGetLine_ : InStreamProvider -> String
hGetLine_     = hGenericGet_ @String hGetLine



-- $$$$$$$\  
-- $$  ____| 
-- $$ |      
-- $$$$$$$\  
-- \_____$$\ 
-- $$\   $$ |
-- \$$$$$$  |
--  \______/ 

-- | Stdout

stdout : OutStreamProvider

putChar : Char -> ()
putChar = flip @Char @OutStreamProvider @() hPutChar_ stdout

putStr, putStrLn : String -> ()
putStr   = flip @String @OutStreamProvider @() hPutStr_ stdout
putStrLn = flip @String @OutStreamProvider @() hPutStrLn_ stdout

print : forall a . a -> ()
-- print = putStrLn $ show @a
print x = putStrLn $ show @a x

-- Internal stdout functions
__runStdout  : dualof OutStreamProvider -> ()
__runStdout = runServer @OutStream @() __runPrinter ()

__runPrinter : () -> dualof OutStream 1-> ()
__runPrinter _ printer =
    match printer with {
        PutChar  printer -> consume @Char   @dualof OutStream (\c:Char -> __putStrOut (show @Char c)) printer & __runPrinter (),
        PutStr   printer -> consume @String @dualof OutStream __putStrOut printer & __runPrinter (),
        PutStrLn printer -> consume @String @dualof OutStream (\s:String -> __putStrOut (s ++ "\n")) printer & __runPrinter (),
        Close    _       -> ()
    }

-- | Stderr

stderr : OutStreamProvider

-- Internal stderr functions
__runStderr  : dualof OutStreamProvider -> ()
__runStderr = runServer @OutStream @() __runErrPrinter ()

__runErrPrinter : () -> dualof OutStream 1-> ()
__runErrPrinter _ printer =
    match printer with {
        PutChar  printer -> consume @Char   @dualof OutStream (\c:Char -> __putStrErr (show @Char c)) printer & __runErrPrinter (),
        PutStr   printer -> consume @String @dualof OutStream __putStrErr printer & __runErrPrinter (),
        PutStrLn printer -> consume @String @dualof OutStream (\s:String -> __putStrErr (s ++ "\n")) printer & __runErrPrinter (),
        Close       _       -> ()
    }

-- | Stdin

stdin : InStreamProvider

getChar : Char
getChar = hGetChar_ stdin

getLine : String
getLine = hGetLine_ stdin

-- Internal stdin functions
__runStdin : dualof InStreamProvider -> ()
__runStdin = runServer @InStream @() __runReader ()

__runReader : () -> dualof InStream 1-> ()
__runReader _ reader = 
    match reader with {
        GetChar reader -> __runReader () $ send (__getChar     ()) reader,
        GetLine reader -> __runReader () $ send (__getLine     ()) reader,
        IsEOF   reader -> __runReader () $ send False reader, -- stdin is always open
        Close   _ -> ()
    }



--  $$$$$$\  
-- $$  __$$\ 
-- $$ /  \__|
-- $$$$$$$\  
-- $$  __$$\ 
-- $$ /  $$ |
--  $$$$$$  |
--  \______/ 

-- | Files

type FilePath = String
data FileHandle = FileHandle () 

data IOMode = ReadMode | WriteMode | AppendMode

openWriteFile : FilePath -> OutStream
openWriteFile fp =
    forkWith @OutStream $ runWriteFile (__openFile fp WriteMode)

openAppendFile : FilePath -> OutStream
openAppendFile fp =
    forkWith @OutStream $ runWriteFile (__openFile fp AppendMode)

runWriteFile : FileHandle -> dualof OutStream 1-> ()
runWriteFile fh ch =
    match ch with {
        PutChar  ch -> let (c, ch) = receive ch in __putFileStr fh (show @Char c); runWriteFile fh ch,
        PutStr   ch -> let (s, ch) = receive ch in __putFileStr fh s             ; runWriteFile fh ch,
        PutStrLn ch -> let (s, ch) = receive ch in __putFileStr fh (s ++ "\n")   ; runWriteFile fh ch,
        Close    _  -> __closeFile fh
    }


openReadFile : FilePath -> InStream
openReadFile fp = 
    forkWith @InStream $ runReadFile (__openFile fp ReadMode)

runReadFile : FileHandle -> dualof InStream 1-> ()
runReadFile fh ch =
    match ch with {
        GetChar ch -> runReadFile fh $ send (__readFileChar fh) ch,
        GetLine ch -> runReadFile fh $ send (__readFileLine fh) ch,
        IsEOF   ch -> runReadFile fh $ send (__isEOF fh       ) ch,
        Close   _  -> __closeFile fh
    }

writeFile : FilePath -> String -> ()
writeFile fp content = openWriteFile fp
                     & hPutStr content
                     & hCloseOut

appendFile : FilePath -> String -> ()
appendFile fp content = openAppendFile fp
                     & hPutStr content
                     & hCloseOut

readFile : FilePath -> String
readFile fp = __fullRead $ openReadFile fp

__fullRead : InStream -> String
__fullRead ch = 
  let (isEOF, ch) = hIsEOF ch in
  if isEOF
  then hCloseIn ch; ""
  else let (line, ch) = hGetLine ch in line ++ "\n" ++ (__fullRead ch)
