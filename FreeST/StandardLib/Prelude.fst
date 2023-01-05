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



-- | Signatures for the builtin operators

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
-- Function call
(|>) : ∀a:*T. ∀b:*T. a -> (a -> b) -> b
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
fork : ∀a:*T. (() 1-> a) -> ()
  -- Error & Undefined
error : ∀a:*T . String -> a
undefined : ∀a:*T . a
  -- Session ops
new : ∀a:1S . () -> (a, dualof a)
send : ∀a:1T . a -> ∀b:1S . !a;b 1-> b
receive : ∀a:1T . ∀b:1S . ?a;b -> (a, b)
close : End -> ()
  -- Not the actual type for collect, but for writing it we would
  -- need polymorphism over the labels in some choice/variant
collect : ∀a:*T . a
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

-- | The identity function
id : ∀a . a -> a
id x = x

-- | Swap the order of parameters to a function
flip : ∀a b c . (a -> b -> c) -> b -> a -> c
flip f x y = f y x

until : ∀a . (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until @a p f (f x)

-- | Convert a function that receives a pair into a function that receives its
-- arguments one at a time.
curry : ∀a b c . ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

-- | Convert a function that receives its arguments one at a time into a function
-- on pairs.
uncurry : ∀a b c . (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst@a @b p) (snd @a @b p)

-- | Swap the components of a pair.
swap : ∀a b . (a, b) -> (b, a)
swap x = let (a, b) = x in (b, a)

-- | Fixed-point Z combinator
fix : ∀a . ((a -> a) -> (a -> a)) -> (a -> a)
fix f =
  (λx:(μb.b -> (a -> a)) -> f (λz:a -> x x z))
  (λx:(μb.b -> (a -> a)) -> f (λz:a -> x x z))



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
-- diverge : Diverge
-- diverge = diverge

-- | Discard an unrestricted value
sink : ∀a . a -> ()
sink _ = ()

-- | Execute a thunk n times, sequentially
repeat : ∀a:*T . Int -> (() -> a) -> ()
repeat n thunk =
    if n <= 0
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

-- | Receive a value from a channel that continues to End, close the 
-- continuation and return the value.
receiveAndClose : ∀a:1T . ?a;End -> a 
receiveAndClose c =
    let (x, c) = receive c in 
    close c;
    x

-- | Receive a value from a star channel
receive_ : ∀a:1T . *?a -> a
receive_ ch = ch |> receive |> fst @a @*?a

-- | Send a value on a star channel
send_ : ∀a:1T . a -> *!a 1-> ()
send_ x ch = ch |> send x |> sink @*!a

-- | Fork n identical threads
parallel : ∀a:*T . Int -> (() -> a) -> ()
parallel n thunk = repeat @() n (λ_:() -> fork @a thunk)

-- | Create a new child process and a linear channel through which it can
-- communicate with its parent process. Return the channel endpoint.
forkWith : ∀a:1S b:*T . (dualof a 1-> b) -> a
forkWith f =
    let (x, y) = new @a () in
    fork (λ_:() 1-> f y);
    x

-- | Session initiation. Accept a request for a linear session on a shared
-- channel. The requester uses a conventional receive to obtain the channel end.
accept : ∀a:1S . *!a -> dualof a
accept ch =
    let (x, y) = new @a () in
    send x ch;
    y

-- | Run a server process given a function to serve a client (a handle), the
-- initial state and the server's endpoint.
runServer : ∀a:1S b . (b -> dualof a 1-> b) -> b -> *!a -> Diverge
runServer handle state ch =
    runServer @a @b handle (handle state (accept @a ch)) ch 



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
                       , Close   : End
                       }

hCloseOut : OutStream -> ()
hCloseOut ch = select Close ch |> close


__hGenericPut : forall a . (OutStream -> !a;OutStream) -> a -> OutStream -> OutStream
__hGenericPut sel x outStream = sel outStream |> send x

hPutChar : Char -> OutStream -> OutStream
hPutChar = __hGenericPut @Char (\ch:OutStream -> select PutChar ch) -- (select PutChar)

hPutStr, hPutStrLn : String -> OutStream -> OutStream
hPutStr   = __hGenericPut @String (\ch:OutStream -> select PutStr   ch) -- (select PutStr)
hPutStrLn = __hGenericPut @String (\ch:OutStream -> select PutStrLn ch) -- (select PutStrLn)

hPrint : forall a . a -> OutStream -> OutStream
hPrint x = hPutStrLn (show @a x)

__hGenericPut_ : forall a . (a -> OutStream -> OutStream) -> a -> OutStreamProvider -> ()
__hGenericPut_ putF x outProv = 
    hCloseOut $ putF x $ receive_ @OutStream outProv 

hPutChar_ : Char -> OutStreamProvider -> ()
hPutChar_ = __hGenericPut_ @Char hPutChar

hPutStr_, hPutStrLn_ : String -> OutStreamProvider -> ()
hPutStr_   = __hGenericPut_ @String hPutStr
hPutStrLn_ = __hGenericPut_ @String hPutStrLn

hPrint_ : forall a . a -> OutStreamProvider -> ()
-- hPrint_ = __hGenericPut_ @a (hPrint @a)
hPrint_ x ch = __hGenericPut_ @a (hPrint @a) x ch


-- | InStream

type InStreamProvider : *S = *?InStream
type InStream : 1S = +{ GetChar     : ?Char  ; InStream
                      , GetLine     : ?String; InStream
                      , IsEOF       : ?Bool  ; InStream
                      , Close       : End
                      }

hCloseIn : InStream -> ()
hCloseIn ch = select Close ch |> close

__hGenericGet : forall a . (InStream -> ?a;InStream) -> InStream -> (a, InStream)
__hGenericGet sel ch = receive $ sel ch

hGetChar : InStream -> (Char, InStream)
hGetChar = __hGenericGet @Char (\ch:InStream -> select GetChar ch)

hGetLine : InStream -> (String, InStream)
hGetLine = __hGenericGet @String (\ch:InStream -> select GetLine ch)

hGetContent : InStream -> (String, InStream)
hGetContent ch = 
  let (isEOF, ch) = hIsEOF ch in
  if isEOF
  then ("", ch)
  else 
    let (line, ch) = hGetLine ch in 
    let (contents, ch) = hGetContent ch in
    (line ++ "\n" ++ contents, ch)

hIsEOF : InStream -> (Bool, InStream)
hIsEOF = __hGenericGet @Bool (\ch:InStream -> select IsEOF ch)

__hGenericGet_ : forall a . (InStream -> (a, InStream)) -> InStreamProvider -> a
__hGenericGet_ getF inp = 
  let (x, ch) = getF $ receive_ @InStream inp in
  let _ = hCloseIn ch in x

hGetChar_ : InStreamProvider -> Char
hGetChar_ = __hGenericGet_ @Char hGetChar

hGetLine_ : InStreamProvider -> String
hGetLine_ = __hGenericGet_ @String hGetLine

hGetContent_ : InStreamProvider -> String
hGetContent_ inp = 
  let (s, c) = receive_ @InStream inp |> hGetContent in
  hCloseIn c;
  s


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
        PutChar  printer -> consume @Char   @dualof OutStream (\c:Char -> __putStrOut (show @Char c)) printer |> __runPrinter (),
        PutStr   printer -> consume @String @dualof OutStream __putStrOut printer |> __runPrinter (),
        PutStrLn printer -> consume @String @dualof OutStream (\s:String -> __putStrOut (s ++ "\n")) printer |> __runPrinter (),
        Close    printer -> close printer
    }

-- | Stderr

stderr : OutStreamProvider

-- Internal stderr functions
__runStderr  : dualof OutStreamProvider -> ()
__runStderr = runServer @OutStream @() __runErrPrinter ()

__runErrPrinter : () -> dualof OutStream 1-> ()
__runErrPrinter _ printer =
    match printer with {
        PutChar  printer -> consume @Char   @dualof OutStream (\c:Char -> __putStrErr (show @Char c)) printer |> __runErrPrinter (),
        PutStr   printer -> consume @String @dualof OutStream __putStrErr printer |> __runErrPrinter (),
        PutStrLn printer -> consume @String @dualof OutStream (\s:String -> __putStrErr (s ++ "\n")) printer |> __runErrPrinter (),
        Close    printer -> close printer
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
        Close   reader -> close reader
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