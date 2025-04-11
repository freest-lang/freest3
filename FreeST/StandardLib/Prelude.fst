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

-- # Builtin

-- Signatures for the builtin operators

-- Int
(+) : Int ->[top,bot] Int ->[top,bot] Int
(-) : Int ->[top,bot] Int ->[top,bot] Int
(*) : Int ->[top,bot] Int ->[top,bot] Int
(/) : Int ->[top,bot] Int ->[top,bot] Int
div : Int ->[top,bot] Int ->[top,bot] Int
(^) : Int ->[top,bot] Int ->[top,bot] Int
mod : Int ->[top,bot] Int ->[top,bot] Int
rem : Int ->[top,bot] Int ->[top,bot] Int
max : Int ->[top,bot] Int ->[top,bot] Int
min : Int ->[top,bot] Int ->[top,bot] Int
quot : Int ->[top,bot] Int ->[top,bot] Int
gcd : Int ->[top,bot] Int ->[top,bot] Int
lcm : Int ->[top,bot] Int ->[top,bot] Int
subtract : Int ->[top,bot] Int ->[top,bot] Int
succ : Int ->[top,bot] Int
pred : Int ->[top,bot] Int
abs : Int ->[top,bot] Int
negate : Int ->[top,bot] Int
even : Int ->[top,bot] Bool
odd : Int ->[top,bot] Bool
(==) : Int ->[top,bot] Int ->[top,bot] Bool
(/=) : Int ->[top,bot] Int ->[top,bot] Bool
(<) : Int ->[top,bot] Int ->[top,bot] Bool
(>) : Int ->[top,bot] Int ->[top,bot] Bool
(<=) : Int ->[top,bot] Int ->[top,bot] Bool
(>=) : Int ->[top,bot] Int ->[top,bot] Bool

-- Float
(+.) : Float ->[top,bot] Float ->[top,bot] Float
(-.) : Float ->[top,bot] Float ->[top,bot] Float
(*.) : Float ->[top,bot] Float ->[top,bot] Float
(/.) : Float ->[top,bot] Float ->[top,bot] Float
(>.) : Float ->[top,bot] Float ->[top,bot] Bool
(<.) : Float ->[top,bot] Float ->[top,bot] Bool
(>=.) : Float ->[top,bot] Float ->[top,bot] Bool
(<=.) : Float ->[top,bot] Float ->[top,bot] Bool
absF : Float ->[top,bot] Float
negateF : Float ->[top,bot] Float
maxF : Float ->[top,bot] Float ->[top,bot] Float
minF : Float ->[top,bot] Float ->[top,bot] Float
truncate : Float ->[top,bot] Int
round : Float ->[top,bot] Int
ceiling : Float ->[top,bot] Int
floor : Float ->[top,bot] Int
recip : Float ->[top,bot] Float
pi : Float
exp : Float ->[top,bot] Float
log : Float ->[top,bot] Float
sqrt : Float ->[top,bot] Float
(**) : Float ->[top,bot] Float ->[top,bot] Float
logBase : Float ->[top,bot] Float ->[top,bot] Float
sin : Float ->[top,bot] Float
cos : Float ->[top,bot] Float
tan : Float ->[top,bot] Float
asin: Float ->[top,bot] Float
acos: Float ->[top,bot] Float
atan: Float ->[top,bot] Float
sinh: Float ->[top,bot] Float
cosh: Float ->[top,bot] Float
tanh: Float ->[top,bot] Float
log1p: Float ->[top,bot] Float
expm1: Float ->[top,bot] Float
log1pexp: Float ->[top,bot] Float
log1mexp: Float ->[top,bot] Float
fromInteger: Int ->[top,bot] Float

-- Bool
(&&) : Bool ->[top,bot] Bool ->[top,bot] Bool
(||) : Bool ->[top,bot] Bool ->[top,bot] Bool

-- Char
ord : Char ->[top,bot] Int
chr : Int ->[top,bot] Char

-- String
(^^) : String ->[top,bot] String ->[top,bot] String
show : forall a:*T . a ->[top,bot] String
-- read : ∀ a . String ->[top,bot] a
readBool : String ->[top,bot] Bool
readInt : String ->[top,bot] Int
readChar : String ->[top,bot] Char

-- Internal Prints
__putStrOut : String ->[top,bot] ()
__putStrErr : String ->[top,bot] ()

-- Internal Gets
__getChar : () ->[top,bot] Char
__getLine : () ->[top,bot] String
__getContents : () ->[top,bot] String

-- Fork
fork : forall a:*T. (() 1->[top,bot] a) ->[top,bot] ()

-- Error & Undefined
error : forall a:*T . String ->[top,bot] a
undefined : forall a:*T . a

-- Session operations
-- | Creates two endpoints of a channels of the given type.
new : forall a:1A . () ->[top,bot] (a, dualof a)
-- | Sends a value on a channel. Returns the continuation channel
send : forall a:1T . a ->[top,bot] forall b:1S . !100a; b 1->[100,100] b
-- | Receives a value on a channel. Returns the received value and 
-- | the continuation channel.
receive : forall a:1T b:1S . ?200a ; b ->[top,200] (a, b)
-- | Closes a channel.
close : Close 300 ->[300,bot] () --n,n or top,n
-- | Waits for a channel to be closed.
wait : Wait 400 ->[400,bot] () --n,n or top,n

-- Files 
-- | File paths
type FilePath = String
-- Internal file handles
data FileHandle = FileHandle ()
-- Internal IOMode for opening files
data IOMode = ReadMode | WriteMode | AppendMode
__openFile : FilePath ->[top,bot] IOMode ->[top,bot] FileHandle
__putFileStr : FileHandle ->[top,bot] String ->[top,bot] ()
__readFileChar : FileHandle ->[top,bot] Char
__readFileLine : FileHandle ->[top,bot] String
__isEOF : FileHandle ->[top,bot] Bool
__closeFile : FileHandle ->[top,bot] ()



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
not : Bool ->[top,bot] Bool 
not True  = False 
not False = True 

-- | Extracts the first element from a pair, discarding the second.
fst : forall a:1T b:*T . (a, b) ->[top,bot] a
fst p = let (x,_) = p in x

-- | Extracts the second element from a pair, discarding the first.
snd : forall a:*T b:1T . (a, b) ->[top,bot] b
snd p = let (_,y) = p in y

-- | The identity function. Will return the exact same value.
-- | ```
-- | id 5       -- 5
-- | id "Hello" -- "Hello"
-- | ```
id : forall a:*T . a ->[top,bot] a
id x = x

-- | Swaps the order of parameters to a function
-- | ```
-- |  -- | Check if the integer is positive and the boolean is true
-- |  test : Int -> Bool -> Bool
-- |  test i b = i > 0 && b
-- |  
-- |  -- | Flipped version of function 'test'
-- |  flippedTest : Bool -> Int -> Bool
-- |  flippedTest = flip @Int @Bool @Bool test
-- |  ```
flip : forall a:*T b:*T c:*T . (a ->[top,bot] b ->[top,bot] c) ->[top,bot] b ->[top,bot] a ->[top,bot] c
flip f x y = f y x

-- | Application operator. Takes a function and an argument, and applies 
-- | the first to the latter. This operator has low right-associative binding 
-- | precedence, allowing parentheses to be omitted in certain situations.
-- | For example:
-- | ```
-- | f $ g $ h x = f (g (h x))
-- | ```
($) : forall a:*T b:*T. (a ->[top,bot] b) ->[top,bot] a ->[top,bot] b 
($) f x = f x

-- | Reverse application operator. Provides notational convenience, especially
-- | when chaining channel operations. For example:
-- | ```
-- | f : !Int ; !Bool ; Close -> () 
-- | f c = c |> send 5 |> send True |> close
-- | ```
-- | Its binding precedence is higher than `$`.
(|>) : forall a:*T b:*T. a ->[top,bot] (a ->[top,bot] b) ->[top,bot] b
(|>) x f = f x

-- | Sequential composition. Takes two expressions, evaluates the former and
-- | discards the result, then evaluates the latter. For example:
-- | ```
-- | 3 ; 4
-- | ```
-- | evaluates to 4.
-- | Its binding precedence is rather low.
(;) : forall a:*T b:*T . a ->[top,bot] b ->[top,bot] b
(;) x y = (\_:a -> y) x --TODO

-- | Applies the function passed as the second argument to the third one and
-- | uses the predicate in the first argument to evaluate the result, if it comes
-- | as True it returns it, otherwise, it continues to apply the function on
-- | previous results until the predicate evaluates to True.
-- | 
-- | ```
-- | -- | First base 2 power greater than a given limit
-- | firstPowerGreaterThan : Int -> Int
-- | firstPowerGreaterThan limit = until @Int (> limit) (*2) 1
-- | ```  
until : forall a:*T . (a ->[top,bot] Bool) ->[top,bot] (a ->[top,bot] a) ->[top,bot] a ->[top,bot] a
until p f x = if p x then x else until @a p f (f x)

-- | Converts a function that receives a pair into a function that receives its
-- | arguments one at a time.
-- | 
-- | ```
-- | -- | Sums the elements of a pair of integers
-- | sumPair : (Int, Int) -> Int
-- | sumPair p = let (x, y) = p in x + y
-- | 
-- | -- | Regular sum
-- | sum : Int -> Int -> Int
-- | sum = curry @Int @Int @Int sumPair
-- | ```
curry : forall a:*T b:*T c:*T . ((a, b) ->[top,bot] c) ->[top,bot] a ->[top,bot] b ->[top,bot] c
curry f x y = f (x, y)

-- | Converts a function that receives its arguments one at a time into a
-- | function on pairs.
-- | 
-- | ```
-- | -- | Sums the elements of a pair of integers
-- | sumPair : (Int, Int) -> Int
-- | sumPair = uncurry @Int @Int @Int (+)
-- | ```
uncurry : forall a:*T b:*T c:*T . (a ->[top,bot] b ->[top,bot] c) ->[top,bot] ((a, b) ->[top,bot] c)
uncurry f p = f (fst@a @b p) (snd @a @b p)

-- | Swaps the components of a pair. The expression `swap (1, True)` evaluates to
-- | `(True, 1)`.
swap : forall a:*T b:*T . (a, b) ->[top,bot] (b, a)
swap x = let (a, b) = x in (b, a)

-- | Fixed-point Z combinator
fix : forall a:*T . ((a ->[top,bot] a) ->[top,bot] (a ->[top,bot] a)) ->[top,bot] (a ->[top,bot] a)
fix f =
  (\x:(rec b:*T . b ->[top,bot] (a ->[top,bot] a)) -> f (\z:a -> x x z)) --TODO
  (\x:(rec b:*T . b ->[top,bot] (a ->[top,bot] a)) -> f (\z:a -> x x z)) --TODO

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

-- | Discards an unrestricted value
sink : forall a:*T . a ->[top,bot] ()
sink _ = ()

-- | Executes a thunk n times, sequentially
-- | 
-- | ```
-- | main : ()
-- | main = 
-- |   -- print "Hello!" 5 times sequentially
-- |   repeat @() 5 (\_:() -> putStrLn "Hello!")
-- | ```
repeat : forall a:*T . Int ->[top,bot] (() ->[top,bot] a) ->[top,bot] ()
repeat n thunk =
  if n <= 0
  then ()
  else 
    thunk ();
    repeat @a (n - 1) thunk

-- | Forks n identical threads. Works the same as a `repeat` call but in parallel
-- | instead of sequentially.
-- | 
-- | ```
-- | main : ()
-- | main = 
-- |   -- print "Hello!" 5 times in parallel
-- |   parallel @() 5 (\_:() -> putStrLn "Hello!")
-- | ```
parallel : forall a:*T . Int ->[top,bot] (() ->[top,bot] a) ->[top,bot] ()
parallel n thunk = repeat @() n (\_:() -> fork @a thunk) --TODO

-- -- type Consumer a = a 1-> ()

-- -- | Receives a value from a linear channel and applies a function to it.
-- -- | Discards the result and returns the continuation channel.
-- -- | 
-- -- | ```
-- -- | main : ()
-- -- | main =
-- -- |   -- create channel endpoints
-- -- |   let (c, s) = new @(?String ; Wait) () in
-- -- |   -- fork a thread that prints the received value (and closes the channel)
-- -- |   fork (\_:() 1-> c |> readApply @String @End putStrLn |> wait);
-- -- |   -- send a string through the channel (and close it)
-- -- |   s |> send "Hello!" |> close
-- -- | ```
-- readApply : forall a:*T b:1S . (a ->[top,bot] ()) {- Consumer a -} ->[top,bot] ?280a ; b 1->[top,280] b --priority?
-- readApply f c =
--   let (x, c) = receive c in
--   f x;
--   c

-- -- | Receives a value from a channel that continues to `Wait`, closes the 
-- -- | continuation and returns the value.
-- -- | 
-- -- | ```
-- -- | main : ()
-- -- | main =
-- -- |   -- create channel endpoints
-- -- |   let (c, s) = new @(?String ; Wait) () in
-- -- |   -- fork a thread that prints the received value (and closes the channel)
-- -- |   fork (\_:() 1-> c |> receiveAndWait @String |> putStrLn);
-- -- |   -- send a string through the channel (and close it)
-- -- |   s |> send "Hello!" |> close
-- -- | ```
-- receiveAndWait : forall a:1T . ?260a ; Wait 261 ->[top,bot] a
-- receiveAndWait c =
--   let (x, c) = receive c in 
--   wait c;
--   x

-- -- | As in receiveAndWait only that the type is Wait and the function closes the
-- -- | channel rather the waiting for the channel to be closed.
-- receiveAndClose : forall a:1T . ?270a ; Close 271 ->[top,bot] a 
-- receiveAndClose c =
--   let (x, c) = receive c in 
--   close c;
--   x

-- -- | Sends a value on a given channel and then waits for the channel to be
-- -- | closed. Returns ().
-- sendAndWait : forall a:1T . a ->[top,bot] !160a ; Wait 161 1->[top,bot] ()
-- sendAndWait x c = c |> send x |> wait

-- -- | Sends a value on a given channel and then closes the channel.
-- -- | Returns ().
-- sendAndClose : forall a:1T . a ->[top,bot] !170a ; Close 171 1->[top,bot] ()
-- sendAndClose x c = c |> send x |> close

-- -- | Receives a value from a star channel. Unrestricted version of `receive`.
-- receive_ : forall a:1T . *?250a ->[top,250] a
-- receive_ c = c |> receive |> fst @a @*?251a

-- -- | Sends a value on a star channel. Unrestricted version of `send`.
-- send_ : forall a:1T . a ->[top,bot] *!150a 1->[150,150] ()
-- send_ x c = c |> send x |> sink @*!151a

-- -- | Session initiation. Accepts a request for a linear session on a shared
-- -- | channel. The requester uses a conventional `receive` to obtain the channel
-- -- | end.
-- accept : forall a : 1A . *!a -> dualof a
-- accept c =
--   let (x, y) = new @a () in
--   send x c;
--   y

-- -- | Creates a new child process and a channel through which it can
-- -- | communicate with its parent process. Returns the channel endpoint.
-- -- | 
-- -- | ```
-- -- | main : ()
-- -- | main =
-- -- |   -- fork a thread that receives a string and prints
-- -- |   let c = forkWith @(!String ; Wait) @() (\s:(?String ; End) 1-> s |> receiveAndWait @String |> putStrLn) in
-- -- |   -- send the string to be printed
-- -- |   c |> send "Hello!" |> wait
-- -- | ```
-- forkWith : forall a:1A b:*T . (dualof a 1-> b) -> a
-- forkWith f =
--   let (x, y) = new @a () in
--   fork (\_:() 1-> f y);
--   x

-- -- | Runs an infinite shared server thread given a function to serve a client (a
-- -- | handle), the initial state, and the server's shared channel endpoint. It can
-- -- | be seen as an infinite sequential application of the handle function over a
-- -- | newly accepted session, while continuously updating the state.
-- -- |   
-- -- | Note: this only works with session types that use session initiation.
-- -- | 
-- -- | ```
-- -- | type SharedCounter : *S = *?Counter
-- -- | type Counter : 1S = +{ Inc: Close
-- -- |                      , Dec: Close
-- -- |                      , Get: ?Int ; Close
-- -- |                      }
-- -- | 
-- -- | -- | Handler for a counter
-- -- | counterService : Int -> dualof Counter 1-> Int
-- -- | counterService i (Inc c) = wait c ; i + 1 
-- -- | counterService i (Dec c) = wait c ; i - 1
-- -- | counterService i (Get c) = c |> send i |> wait ; i
-- -- | 
-- -- | -- | Counter server
-- -- | runCounterServer : dualof SharedCounter -> Diverge
-- -- | runCounterServer = runServer @Counter @Int counterService 0 
-- -- | ```
-- runServer : forall a:1A b:*T . (b -> dualof a 1-> b) -> b -> *!a -> Diverge
-- runServer handle state c =
--   runServer @a @b handle (handle state (accept @a c)) c 



-- -- $$\   $$\ 
-- -- $$ |  $$ |
-- -- $$ |  $$ |
-- -- $$$$$$$$ |
-- -- \_____$$ |
-- --       $$ |
-- --       $$ |
-- --       \__|

-- -- # Output and input streams

-- -- | The `OutStream` type describes output streams (such as `stdout`, `stderr`
-- -- | and write mode files). `PutChar` outputs a character, `PutStr` outputs a string,
-- -- | and `PutStrLn` outputs a string followed by the newline character (`\n`).
-- -- | Operations in this channel must end with the `Close` option.
-- type OutStream : 1S = +{ PutChar : !Char ; OutStream
--                        , PutStr  : !String ; OutStream
--                        , PutStrLn: !String ; OutStream
--                        , SWait   : Wait
--                        }

-- -- | Unrestricted session type for the `OutStream` type.
-- type OutStreamProvider : *A = *?OutStream

-- -- | Closes an `OutStream` channel endpoint. Behaves as a `close`.
-- hCloseOut : OutStream -> ()
-- hCloseOut c = c |> select SWait |> wait

-- __hGenericPut : forall a:*T . (OutStream -> !a ; OutStream) -> a -> OutStream -> OutStream
-- __hGenericPut sel x outStream = sel outStream |> send x

-- -- | Sends a character through an `OutStream` channel endpoint. Behaves as 
-- -- | `|> select PutChar |> send`.
-- hPutChar : Char -> OutStream -> OutStream
-- hPutChar = __hGenericPut @Char (\ch:OutStream -> select PutChar ch)

-- -- | Sends a String through an `OutStream` channel endpoint. Behaves as 
-- -- | `|> select PutString |> send`.
-- hPutStr : String -> OutStream -> OutStream
-- hPutStr   = __hGenericPut @String (\c:OutStream -> select PutStr c)

-- -- | Sends a string through an `OutStream` channel endpoint, to be output with
-- -- | the newline character. Behaves as `|> select PutStringLn |> send`.
-- hPutStrLn : String -> OutStream -> OutStream
-- hPutStrLn = __hGenericPut @String (\c:OutStream -> select PutStrLn c)

-- -- | Sends the string representation of a value through an `OutStream` channel
-- -- | endpoint, to be outputed with the newline character. Behaves as `hPutStrLn
-- -- | (show @t v)`, where `v` is the value to be sent and `t` its type.
-- hPrint : forall a:*T . a -> OutStream -> OutStream
-- hPrint x = hPutStrLn (show @a x)

-- __hGenericPut_ : forall a : *T . (a -> OutStream -> OutStream) -> a -> OutStreamProvider -> ()
-- __hGenericPut_ putF x outProv = 
--   hCloseOut $ putF x $ receive_ @OutStream outProv 

-- -- | Unrestricted version of `hPutChar`. Behaves the same, except it first
-- -- | receives an `OutStream` channel endpoint (via session initiation), executes
-- -- | an `hPutChar` and then closes the enpoint with `hCloseOut`.
-- hPutChar_ : Char -> OutStreamProvider -> ()
-- hPutChar_ = __hGenericPut_ @Char hPutChar

-- -- | Unrestricted version of `hPutStr`. Behaves similarly, except that it first
-- -- | receives an `OutStream` channel endpoint (via session initiation), executes
-- -- | an `hPutStr` and then closes the enpoint with `hCloseOut`.
-- hPutStr_ : String -> OutStreamProvider -> ()
-- hPutStr_ = __hGenericPut_ @String hPutStr

-- -- | Unrestricted version of `hPutStrLn`. Behaves similarly, except that it
-- -- | first receives an `OutStream` channel endpoint (via session initiation),
-- -- | executes an `hPutStrLn` and then closes the enpoint with `hCloseOut`.
-- hPutStrLn_ : String -> OutStreamProvider -> ()
-- hPutStrLn_ = __hGenericPut_ @String hPutStrLn

-- -- | Unrestricted version of `hPrint`. Behaves similarly, except that it first
-- -- | receives an `OutStream` channel endpoint (via session initiation), executes
-- -- | an `hPrint` and then closes the enpoint with `hCloseOut`.
-- hPrint_ : forall a:*T . a -> OutStreamProvider -> ()
-- hPrint_ x c = __hGenericPut_ @a (hPrint @a) x c


-- -- InStream

-- -- | The `InStream` type describes input streams (such as `stdin` and read
-- -- | files). `GetChar` reads a single character, `GetLine` reads a line, and
-- -- | `IsEOF` checks for the EOF (End-Of-File) token, i.e., if an input stream
-- -- | reached the end. Operations in this channel end with the `SWait` option.
-- type InStream : 1S = +{ GetChar: ?Char   ; InStream
--                       , GetLine: ?String ; InStream
--                       , IsEOF  : ?Bool   ; InStream
--                       , SWait  : Wait
--                       }

-- -- | Unrestricted session type for the `OutStream` type.
-- type InStreamProvider : *A = *?InStream

-- -- | Closes an `InStream` channel endpoint. Behaves as a `close`.
-- hCloseIn : InStream -> ()
-- hCloseIn c = c |> select SWait |> wait

-- __hGenericGet : forall a:*T . (InStream -> ?a ; InStream) -> InStream -> (a, InStream)
-- __hGenericGet sel c = receive $ sel c

-- -- | Reads a character from an `InStream` channel endpoint. Behaves as 
-- -- | `|> select GetChar |> receive`.
-- hGetChar : InStream -> (Char, InStream)
-- hGetChar = __hGenericGet @Char (\c:InStream -> select GetChar c)

-- -- | Reads a line (as a string) from an `InStream` channel endpoint. Behaves as 
-- -- | `|> select GetLine |> receive`.
-- hGetLine : InStream -> (String, InStream)
-- hGetLine = __hGenericGet @String (\c:InStream -> select GetLine c)

-- -- | Checks if an `InStream` reached the EOF token that marks where no more input can be read. 
-- -- | Does the same as `|> select IsEOF |> receive`.
-- hIsEOF : InStream -> (Bool, InStream)
-- hIsEOF = __hGenericGet @Bool (\c:InStream -> select IsEOF c)

-- -- | Reads the entire content from an `InStream` (i.e. until EOF is reached). Returns the content
-- -- | as a single string and the continuation channel.
-- hGetContent : InStream -> (String, InStream)
-- hGetContent c = 
--   let (isEOF, c) = hIsEOF c in
--   if isEOF
--   then ("", c)
--   else 
--     let (line, c) = hGetLine c in 
--     let (contents, c) = hGetContent c in
--     (line ^^ "\n" ^^ contents, c)

-- __hGenericGet_ : forall a:*T . (InStream -> (a, InStream)) -> InStreamProvider -> a
-- __hGenericGet_ getF inp = 
--   let (x, c) = getF $ receive_ @InStream inp in
--   let _ = hCloseIn c in x

-- -- | Unrestricted version of `hGetChar`. Behaves the same, except it first receives an `InStream` 
-- -- | channel endpoint (via session initiation), executes an `hGetChar` and then closes the 
-- -- | enpoint with `hCloseIn`.
-- hGetChar_ : InStreamProvider -> Char
-- hGetChar_ = __hGenericGet_ @Char hGetChar

-- -- | Unrestricted version of `hGetLine`. Behaves the same, except it first receives an `InStream` 
-- -- | channel endpoint (via session initiation), executes an `hGetLine` and then closes the 
-- -- | enpoint with `hCloseIn`.
-- hGetLine_ : InStreamProvider -> String
-- hGetLine_ = __hGenericGet_ @String hGetLine

-- -- | Unrestricted version of `hGetContent`. Behaves the same, except it first receives an `InStream`
-- -- | channel endpoint (via session initiation), executes an `hGetContent` and then closes the
-- -- | endpoint with `hCloseIn`.
-- hGetContent_ : InStreamProvider -> String
-- hGetContent_ inp = 
--   let (s, c) = receive_ @InStream inp |> hGetContent in
--   hCloseIn c;
--   s


-- -- $$$$$$$\  
-- -- $$  ____| 
-- -- $$ |      
-- -- $$$$$$$\  
-- -- \_____$$\ 
-- -- $$\   $$ |
-- -- \$$$$$$  |
-- --  \______/ 

-- -- # Standard IO

-- -- Stdout

-- -- | Standard output stream. Prints to the console.
-- __stdoutChan : (OutStreamProvider, dualof OutStreamProvider)
-- __stdoutChan = new @OutStreamProvider () 

-- stdout : OutStreamProvider
-- stdout = let (o,_) = __stdoutChan in o

-- __stdout : dualof OutStreamProvider
-- __stdout = let (_,i) = __stdoutChan in i

-- -- | Prints a character to `stdout`. Behaves the same as `hPutChar_ c stdout`, where `c`
-- -- | is the character to be printed.
-- putChar : Char -> ()
-- putChar = flip @Char @OutStreamProvider @() hPutChar_ stdout

-- -- | Prints a string to `stdout`. Behaves the same as `hPutStr_ s stdout`, where `s` is
-- -- | the string to be printed.
-- putStr : String -> ()
-- putStr = flip @String @OutStreamProvider @() hPutStr_ stdout

-- -- | Prints a string to `stdout`, followed by the newline character `\n`. Behaves
-- -- | as `hPutStrLn_ s stdout`, where `s` is the string to be printed.
-- putStrLn : String -> ()
-- putStrLn = flip @String @OutStreamProvider @() hPutStrLn_ stdout

-- -- | Prints the string representation of a given value to `stdout`, followed by
-- -- | the newline character `\n`. Behaves the same as `hPrint_ @t v stdout`, where `v` is
-- -- | the value to be printed and `t` its type.
-- print : forall a:*T . a -> ()
-- print x = putStrLn $ show @a x

-- -- Internal stdout functions
-- __runPrinter : () -> dualof OutStream 1-> ()
-- __runPrinter _ (PutChar printer) =
--   readApply @Char @dualof OutStream (\c:Char -> __putStrOut (show @Char c)) printer |> __runPrinter ()
-- __runPrinter _ (PutStr printer) =
--   readApply @String @dualof OutStream __putStrOut printer |> __runPrinter ()
-- __runPrinter _ (PutStrLn printer) =
--   readApply @String @dualof OutStream (\s:String -> __putStrOut (s ^^ "\n")) printer |> __runPrinter ()
-- __runPrinter _ (SWait printer) =
--   close printer

-- __runStdout  : ()
-- __runStdout = fork (\_:() -> runServer @OutStream @() __runPrinter () __stdout)

-- -- Stderr

-- -- | Standard error stream. Prints to the console.
-- __stderrChan : (OutStreamProvider, dualof OutStreamProvider)
-- __stderrChan = new @OutStreamProvider () 

-- stderr : OutStreamProvider
-- stderr = let (o,_) = __stderrChan in o

-- __stderr : dualof OutStreamProvider
-- __stderr = let (_,i) = __stderrChan in i

-- -- Internal stderr functions
-- __runErrPrinter : () -> dualof OutStream 1-> ()
-- __runErrPrinter _ (PutChar  printer) =
--   readApply @Char   @dualof OutStream (\c:Char -> __putStrErr (show @Char c)) printer |> __runErrPrinter ()
-- __runErrPrinter _ (PutStr printer) =
--   readApply @String @dualof OutStream __putStrErr printer |> __runErrPrinter ()
-- __runErrPrinter _ (PutStrLn printer) =
--   readApply @String @dualof OutStream (\s:String -> __putStrErr (s ^^ "\n")) printer |> __runErrPrinter ()
-- __runErrPrinter _ (SWait printer) =
--   close printer

-- __runStderr : ()
-- __runStderr = fork (\_:() -> runServer @OutStream @() __runErrPrinter () __stderr)

-- -- Stdin

-- -- | Standard input stream. Reads from the console.
-- -- | Standard output stream. Prints to the console.
-- __stdinChan : (InStreamProvider, dualof InStreamProvider)
-- __stdinChan = new @InStreamProvider () 

-- stdin : InStreamProvider
-- stdin = let (i,_) = __stdinChan in i

-- __stdin : dualof InStreamProvider
-- __stdin = let (_,o) = __stdinChan in o

-- -- | Reads a single character from `stdin`.
-- getChar : () -> Char
-- getChar _ = hGetChar_ stdin

-- -- | Reads a single line from `stdin`. 
-- getLine : () -> String
-- getLine _ = hGetLine_ stdin

-- -- Internal stdin functions
-- __runReader : () -> dualof InStream 1-> ()
-- __runReader _ (GetChar reader) =
--   __runReader () $ send (__getChar ()) reader
-- __runReader _ (GetLine reader) =
--   __runReader () $ send (__getLine ()) reader
-- __runReader _ (IsEOF reader) =
--   __runReader () $ send False reader -- stdin is always open
-- __runReader _ (SWait reader) =
--   close reader

-- __runStdin : ()
-- __runStdin = fork (\_:() -> runServer @InStream @() __runReader () __stdin)
