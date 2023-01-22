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



-- Signatures for the builtin operators

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

-- # Base

-- | Bool 
data Bool = True | False 

-- | Boolean complement
not : Bool -> Bool 
not True  = False 
not False = True 

-- | The identity function. Will return the exact same value.
-- | ```
-- | id 5       -- 5
-- | id "Hello" -- "Hello"
-- | ```
id : ∀a . a -> a
id x = x

-- | Swap the order of parameters to a function
-- | ```
-- |  -- | Check if the integer is positive and the boolean is true
-- |  test : Int -> Bool -> Bool
-- |  test i b = i > 0 && b
-- |  
-- |  -- | Flipped version of function 'test'
-- |  flippedTest : Bool -> Int -> Bool
-- |  flippedTest = flip @Int @Bool @Bool
-- |  ```
flip : ∀a b c . (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- | Applies the function passed as the second argument to the third one and uses the predicate in the
-- |     first argument to evaluate the result, if it comes as True it returns it, otherwise, it 
-- |     continues to apply the function on previous results until the predicate evaluates to True.
-- | 
-- | ```
-- | -- | First base 2 power greater than a given limit
-- | firstPowerGreaterThan : Int -> Int
-- | firstPowerGreaterThan limit = until (> limit) (*2) 1
-- | ```  
until : ∀a . (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until @a p f (f x)

-- | Convert a function that receives a pair into a function that receives its
-- | arguments one at a time.
-- | 
-- | ```
-- | -- | Sums the elements of a pair of integers
-- | sumPair : (Int, Int) -> Int
-- | sumPair (x, y) = x + y
-- | 
-- | -- | Regular sum
-- | sum : Int -> Int -> Int
-- | sum = curry @Int @Int @Int sumPair
-- | ```
curry : ∀a b c . ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

-- | Convert a function that receives its arguments one at a time into a function
-- | on pairs.
-- | 
-- | ```
-- | -- | Sums the elements of a pair of integers
-- | sumPair : (Int, Int) -> Int
-- | sumPair = uncurry @Int @Int @Int (+)
-- | ```
uncurry : ∀a b c . (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst@a @b p) (snd @a @b p)

-- | Swap the components of a pair. The expression `swap (1, True)` evaluates to `(True, 1)`.
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

-- # Concurrency and channels

-- | A mark for functions that do not terminate
type Diverge = ()

-- A function that diverges
-- diverge : Diverge
-- diverge = diverge

-- | Discard an unrestricted value
sink : ∀a . a -> ()
sink _ = ()

-- | Execute a thunk n times, sequentially
-- | 
-- | ```
-- | main : ()
-- | main = 
-- |     -- print "Hello!" 5 times sequentially
-- |     repeat @() n (\_:() -> putStrLn "Hello!")
-- | ```
repeat : ∀a . Int -> (() -> a) -> ()
repeat n thunk =
    if n <= 0
    then ()
    else 
        thunk ();
        repeat @a (n - 1) thunk

-- | Fork n identical threads. Works the same as a `repeat` call but in parallel instead of 
-- | sequentially.
-- | 
-- | ```
-- | main : ()
-- | main = 
-- |     -- print "Hello!" 5 times in parallel
-- |     parallel @() n (\_:() -> putStrLn "Hello!")
-- | ```
parallel : ∀a . Int -> (() -> a) -> ()
parallel n thunk = repeat @() n (λ_:() -> fork @a thunk)

-- type Consumer a = a 1-> ()

-- | Receive a value from a linear channel and apply a function to it.
-- | Returns the continuation channel
-- | 
-- | ```
-- | main : ()
-- | main =
-- |     -- create channel endpoints
-- |     let (c, s) = new ?String; End in
-- |     -- fork a thread that prints the received value (and closes the channel)
-- |     fork (\_:() -> consume @String @End putStrLn c |> close);
-- |     -- send a string through the channel (and close it)
-- |     send "Hello!" s |> close
-- | ```
consume : forall a b:1S . (a -> ()) {- Consumer a -} -> ?a;b 1-> b
consume f ch =
    let (x, ch) = receive ch in
    f x;
    ch

-- | Receive a value from a channel that continues to End, close the 
-- | continuation and return the value.
-- | 
-- | ```
-- | main : ()
-- | main =
-- |     -- create channel endpoints
-- |     let (c, s) = new ?String; End in
-- |     -- fork a thread that prints the received value (and closes the channel)
-- |     fork (\_:() -> putStrLn $ receiveAndClose @String c);
-- |     -- send a string through the channel (and close it)
-- |     send "Hello!" s |> close
-- | ```
receiveAndClose : ∀a:1T . ?a;End -> a 
receiveAndClose c =
    let (x, c) = receive c in 
    close c;
    x

-- | Receive a value from a star channel. Unrestricted version of `receive`.
receive_ : ∀a:1T . *?a -> a
receive_ ch = ch |> receive |> fst @a @*?a

-- | Send a value on a star channel. Unrestricted version of `send`.
send_ : ∀a:1T . a -> *!a 1-> ()
send_ x ch = ch |> send x |> sink @*!a

-- | Create a new child process and a linear channel through which it can
-- | communicate with its parent process. Return the channel endpoint.
-- | 
-- | ```
-- | main : ()
-- | main =
-- |     -- fork a thread that receives a string and prints
-- |     let c = forkWith @!String;End @() (receiveAndClose @String |> putStrLn) in
-- |     -- send the string to be printed
-- |     send "Hello!"
-- | ```
forkWith : ∀a:1S b . (dualof a 1-> b) -> a
forkWith f =
    let (x, y) = new @a () in
    fork (λ_:() 1-> f y);
    x

-- | Session initiation. Accept a request for a linear session on a shared
-- | channel. The requester uses a conventional receive to obtain the channel end.
accept : ∀a:1S . *!a -> dualof a
accept ch =
    let (x, y) = new @a () in
    send x ch;
    y

-- | Run an infinite shared server thread given a function to serve a client (a handle), the initial 
-- |     state, and the server's shared channel endpoint. It can be seen as an infinite sequential 
-- |     application of the handle function over a newly accepted session, while contiuously updating
-- |     the state. 
-- |     
-- | Note this only works with session types that use session initiation.
-- | 
-- | ```
-- | type SharedCounter : *S = *?Counter
-- | type Counter : 1S = +{ Inc: End
-- |                      , Dec: End
-- |                      , Get: ?Int; End
-- |                      }
-- | 
-- | -- | Handler for a counter
-- | counterService : Int -> dualof Counter 1-> Int
-- | counterService i (Inc ch) = close ch; i+1 
-- | counterService i (Dec ch) = close ch; i-1
-- | counterService i (Get ch) = send i ch |> close; i
-- | 
-- | -- | Counter server
-- | runCounterServer : dualof SharedCounter -> Diverge
-- | runCounterServer = runServer @Counter @Int counterService 0 
-- | ```
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

-- # Output and input streams

-- | The `OutStream` type describes output streams (such as `stdout`, `stderr` and write files).
-- |     `PutStr` outputs a character, `PutStr` outputs a string, and `PutStrLn` outputs a string 
-- |     followed by the newline character (`\n`). Operations in this channel end with the `Close`
-- |     option.
type OutStream : 1S = +{ PutChar : !Char; OutStream
                       , PutStr  : !String; OutStream
                       , PutStrLn: !String; OutStream
                       , Close   : End
                       }

-- | Unrestricted session type for the `OutStream` type.
type OutStreamProvider : *S = *?OutStream

-- | Closes an `OutStream` channel endpoint. Does the same as a `close`.
hCloseOut : OutStream -> ()
hCloseOut ch = select Close ch |> close


__hGenericPut : forall a . (OutStream -> !a;OutStream) -> a -> OutStream -> OutStream
__hGenericPut sel x outStream = sel outStream |> send x

-- | Send a character through an `OutStream` channel endpoint. Does the same as 
-- |     `|> select PutChar |> send`.
hPutChar : Char -> OutStream -> OutStream
hPutChar = __hGenericPut @Char (\ch:OutStream -> select PutChar ch) -- (select PutChar)

-- | Send a String through an `OutStream` channel endpoint. Does the same as 
-- |     `|> select PutString |> send`.
hPutStr : String -> OutStream -> OutStream
hPutStr   = __hGenericPut @String (\ch:OutStream -> select PutStr   ch) -- (select PutStr)

-- | Send a string through an `OutStream` channel endpoint, to be outputed with the newline character. 
-- |     Does the same as `|> select PutStringLn |> send`.
hPutStrLn : String -> OutStream -> OutStream
hPutStrLn = __hGenericPut @String (\ch:OutStream -> select PutStrLn ch) -- (select PutStrLn)

-- | Sends the string representation of a value through an `OutStream` channel endpoint, to be outputed
-- |     with the newline character. Does the same as `hPutStrLn (show @t v)`, where `v` is the value to
-- |     be sent and `t` its type.
hPrint : forall a . a -> OutStream -> OutStream
hPrint x = hPutStrLn (show @a x)

__hGenericPut_ : forall a . (a -> OutStream -> OutStream) -> a -> OutStreamProvider -> ()
__hGenericPut_ putF x outProv = 
    hCloseOut $ putF x $ receive_ @OutStream outProv 

-- | Unrestricted version of `hPutChar`. Behaves the same, except it first receives an `OutStream` 
-- |     channel endpoint (via session initiation), executes an `hPutChar` and then closes the 
-- |     enpoint with `hCloseOut`.
hPutChar_ : Char -> OutStreamProvider -> ()
hPutChar_ = __hGenericPut_ @Char hPutChar

-- | Unrestricted version of `hPutStr`. Behaves the same, except it first receives an `OutStream` 
-- |     channel endpoint (via session initiation), executes an `hPutStr` and then closes the 
-- |     enpoint with `hCloseOut`.
hPutStr_ : String -> OutStreamProvider -> ()
hPutStr_   = __hGenericPut_ @String hPutStr

-- | Unrestricted version of `hPutStrLn`. Behaves the same, except it first receives an `OutStream` 
-- |     channel endpoint (via session initiation), executes an `hPutStrLn` and then closes the 
-- |     enpoint with `hCloseOut`.
hPutStrLn_ : String -> OutStreamProvider -> ()
hPutStrLn_ = __hGenericPut_ @String hPutStrLn

-- | Unrestricted version of `hPrint`. Behaves the same, except it first receives an `OutStream` 
-- |     channel endpoint (via session initiation), executes an `hPrint` and then closes the 
-- |     enpoint with `hCloseOut`.
hPrint_ : forall a . a -> OutStreamProvider -> ()
-- hPrint_ = __hGenericPut_ @a (hPrint @a)
hPrint_ x ch = __hGenericPut_ @a (hPrint @a) x ch


-- InStream

-- | The `InStream` type describes input streams (such as `stdin` and read files).
-- |     `GetChar` reads a single character, `GetLine` reads a line, and `IsEOF` checks for the EOF
-- |     (End-Of-File) token, i.e., if an input stream reached the end. Operations in this channel end 
-- |     with the `Close` option.
type InStream : 1S = +{ GetChar     : ?Char  ; InStream
                      , GetLine     : ?String; InStream
                      , IsEOF       : ?Bool  ; InStream
                      , Close       : End
                      }

-- | Unrestricted session type for the `OutStream` type.
type InStreamProvider : *S = *?InStream

-- | Closes an `InStream` channel endpoint. Does the same as a `close`.
hCloseIn : InStream -> ()
hCloseIn ch = select Close ch |> close

__hGenericGet : forall a . (InStream -> ?a;InStream) -> InStream -> (a, InStream)
__hGenericGet sel ch = receive $ sel ch

-- | Reads a character from an `InStream` channel endpoint. Does the same as 
-- |     `|> select GetChar |> receive`.
hGetChar : InStream -> (Char, InStream)
hGetChar = __hGenericGet @Char (\ch:InStream -> select GetChar ch)

-- | Reads a line (as a string) from an `InStream` channel endpoint. Does the same as 
-- |     `|> select GetLine |> receive`.
hGetLine : InStream -> (String, InStream)
hGetLine = __hGenericGet @String (\ch:InStream -> select GetLine ch)

-- | Checks if an `InStream` reached the EOF token that marks where no more input can be read. Does the
-- |     same as `|> select IsEOF |> receive`.
hIsEOF : InStream -> (Bool, InStream)
hIsEOF = __hGenericGet @Bool (\ch:InStream -> select IsEOF ch)

-- | Reads the entire content from an `InStream` (i.e. until EOF is reached). Returns the content
-- |      as a single string and the continuation channel.
hGetContent : InStream -> (String, InStream)
hGetContent ch = 
  let (isEOF, ch) = hIsEOF ch in
  if isEOF
  then ("", ch)
  else 
    let (line, ch) = hGetLine ch in 
    let (contents, ch) = hGetContent ch in
    (line ++ "\n" ++ contents, ch)

__hGenericGet_ : forall a . (InStream -> (a, InStream)) -> InStreamProvider -> a
__hGenericGet_ getF inp = 
  let (x, ch) = getF $ receive_ @InStream inp in
  let _ = hCloseIn ch in x

-- | Unrestricted version of `hGetChar`. Behaves the same, except it first receives an `InStream` 
-- |     channel endpoint (via session initiation), executes an `hGetChar` and then closes the 
-- |     enpoint with `hCloseIn`.
hGetChar_ : InStreamProvider -> Char
hGetChar_ = __hGenericGet_ @Char hGetChar

-- | Unrestricted version of `hGetLine`. Behaves the same, except it first receives an `InStream` 
-- |     channel endpoint (via session initiation), executes an `hGetLine` and then closes the 
-- |     enpoint with `hCloseIn`.
hGetLine_ : InStreamProvider -> String
hGetLine_ = __hGenericGet_ @String hGetLine

-- | Unrestricted version of `hGetContents`. Behaves the same, except it first receives an `InStream`
-- |     channel endpoint (via session initiation), executes an `hGetContents` and then closes the
-- |     endpoint with `hCloseIn`.
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

-- # Standard IO

-- Stdout

-- | Standard output stream. Prints to the console.
stdout : OutStreamProvider

-- | Print a character to `stdout`. Does the same as `hPutChar_ c stdout`, where `c` is the character
-- |     to be printed.
putChar : Char -> ()
putChar = flip @Char @OutStreamProvider @() hPutChar_ stdout

-- | Print a string to `stdout`. Does the same as `hPutStr_ s stdout`, where `s` is the string to be 
-- |     printed.
putStr : String -> ()
putStr   = flip @String @OutStreamProvider @() hPutStr_ stdout

-- | Print a string to `stdout`, followed by the newline character `\n`. Does the same as 
-- |     `hPutStrLn_ s stdout`, where `s` is the string to be printed.
putStrLn : String -> ()
putStrLn = flip @String @OutStreamProvider @() hPutStrLn_ stdout

-- | Print the string representation of a given value to `stdout`, followed by the newline character 
-- |     `\n`. Does the same as `hPrint_ @t v stdout`, where `v` is the value to be printed and `t`
-- |     its type.
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

-- Stderr

-- | Standard error stream. Prints to the console.
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

-- Stdin

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

-- # File types

-- | File paths.
type FilePath = String

-- Internal file handles
data FileHandle = FileHandle () 

-- Internal IOMode for opening files
data IOMode = ReadMode | WriteMode | AppendMode