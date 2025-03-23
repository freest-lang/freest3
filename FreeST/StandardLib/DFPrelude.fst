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
(+) : Int -> Int -> Int
(-) : Int -> Int -> Int
(*) : Int -> Int -> Int
(/) : Int -> Int -> Int
div : Int -> Int -> Int
(^) : Int -> Int -> Int
mod : Int -> Int -> Int
rem : Int -> Int -> Int
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

-- Float
(+.) : Float -> Float -> Float
(-.) : Float -> Float -> Float
(*.) : Float -> Float -> Float
(/.) : Float -> Float -> Float
(>.) : Float -> Float -> Bool
(<.) : Float -> Float -> Bool
(>=.) : Float -> Float -> Bool
(<=.) : Float -> Float -> Bool
absF : Float -> Float
negateF : Float -> Float
maxF : Float -> Float -> Float
minF : Float -> Float -> Float
truncate : Float -> Int
round : Float -> Int
ceiling : Float -> Int
floor : Float -> Int
recip : Float -> Float
pi : Float
exp : Float -> Float
log : Float -> Float
sqrt : Float -> Float
(**) : Float -> Float -> Float
logBase : Float -> Float -> Float
sin : Float -> Float
cos : Float -> Float
tan : Float -> Float
asin: Float -> Float
acos: Float -> Float
atan: Float -> Float
sinh: Float -> Float
cosh: Float -> Float
tanh: Float -> Float
log1p: Float -> Float
expm1: Float -> Float
log1pexp: Float -> Float
log1mexp: Float -> Float
fromInteger: Int -> Float

-- Bool
(&&) : Bool -> Bool -> Bool
(||) : Bool -> Bool -> Bool

-- Char
ord : Char -> Int
chr : Int -> Char

-- String
(^^) : String -> String -> String
show : forall a:*T . a -> String
-- read : ∀ a . String -> a
readBool : String -> Bool
readInt : String -> Int
readChar : String -> Char

-- Internal Prints
__putStrOut : String -> ()
__putStrErr : String -> ()

-- Internal Gets
__getChar : () -> Char
__getLine : () -> String
__getContents : () -> String

-- Fork
fork : forall a:*T. (() 1-> a) -> ()

-- Error & Undefined
error : forall a:*T . String -> a
undefined : forall a:*T . a

-- Session operations
-- | Creates two endpoints of a channels of the given type.
new : forall a:1A . () -> (a, dualof a)
-- | Sends a value on a channel. Returns the continuation channel
send : forall a:1T . a -> forall b:1S . !a; b 1-> b
-- | Receives a value on a channel. Returns the received value and 
-- | the continuation channel.
receive : forall a:1T b:1S . ?a ; b -> (a, b)
-- | Closes a channel.
close : Close -> ()
-- | Waits for a channel to be closed.
wait : Wait -> ()

-- Files 
-- | File paths
type FilePath = String
-- Internal file handles
data FileHandle = FileHandle ()
-- Internal IOMode for opening files
data IOMode = ReadMode | WriteMode | AppendMode
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

-- | Extracts the first element from a pair, discarding the second.
fst : forall a:1T b:*T . (a, b) -> a
fst p = let (x,_) = p in x

-- | Extracts the second element from a pair, discarding the first.
snd : forall a:*T b:1T . (a, b) -> b
snd p = let (_,y) = p in y

-- | The identity function. Will return the exact same value.
-- | ```
-- | id 5       -- 5
-- | id "Hello" -- "Hello"
-- | ```
id : forall a:*T . a -> a
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
flip : forall a:*T b:*T c:*T . (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- | Application operator. Takes a function and an argument, and applies 
-- | the first to the latter. This operator has low right-associative binding 
-- | precedence, allowing parentheses to be omitted in certain situations.
-- | For example:
-- | ```
-- | f $ g $ h x = f (g (h x))
-- | ```
($) : forall a:*T b:*T. (a -> b) -> a -> b 
($) f x = f x

-- | Reverse application operator. Provides notational convenience, especially
-- | when chaining channel operations. For example:
-- | ```
-- | f : !Int ; !Bool ; Close -> () 
-- | f c = c |> send 5 |> send True |> close
-- | ```
-- | Its binding precedence is higher than `$`.
(|>) : forall a:*T b:*T. a -> (a -> b) -> b
(|>) x f = f x

-- | Sequential composition. Takes two expressions, evaluates the former and
-- | discards the result, then evaluates the latter. For example:
-- | ```
-- | 3 ; 4
-- | ```
-- | evaluates to 4.
-- | Its binding precedence is rather low.
(;) : forall a:*T b:*T . a -> b -> b
(;) x y = (\_:a -> y) x

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
until : forall a:*T . (a -> Bool) -> (a -> a) -> a -> a
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
curry : forall a:*T b:*T c:*T . ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

-- | Converts a function that receives its arguments one at a time into a
-- | function on pairs.
-- | 
-- | ```
-- | -- | Sums the elements of a pair of integers
-- | sumPair : (Int, Int) -> Int
-- | sumPair = uncurry @Int @Int @Int (+)
-- | ```
uncurry : forall a:*T b:*T c:*T . (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst@a @b p) (snd @a @b p)

-- | Swaps the components of a pair. The expression `swap (1, True)` evaluates to
-- | `(True, 1)`.
swap : forall a:*T b:*T . (a, b) -> (b, a)
swap x = let (a, b) = x in (b, a)

-- | Fixed-point Z combinator
fix : forall a:*T . ((a -> a) -> (a -> a)) -> (a -> a)
fix f =
  (\x:(rec b:*T . b -> (a -> a)) -> f (\z:a -> x x z))
  (\x:(rec b:*T . b -> (a -> a)) -> f (\z:a -> x x z))

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
sink : forall a:*T . a -> ()
sink _ = ()

-- | Executes a thunk n times, sequentially
-- | 
-- | ```
-- | main : ()
-- | main = 
-- |   -- print "Hello!" 5 times sequentially
-- |   repeat @() 5 (\_:() -> putStrLn "Hello!")
-- | ```
repeat : forall a:*T . Int -> (() -> a) -> ()
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
parallel : forall a:*T . Int -> (() -> a) -> ()
parallel n thunk = repeat @() n (\_:() -> fork @a thunk)

-- type Consumer a = a 1-> ()

-- | Receives a value from a linear channel and applies a function to it.
-- | Discards the result and returns the continuation channel.
-- | 
-- | ```
-- | main : ()
-- | main =
-- |   -- create channel endpoints
-- |   let (c, s) = new @(?String ; Wait) () in
-- |   -- fork a thread that prints the received value (and closes the channel)
-- |   fork (\_:() 1-> c |> readApply @String @End putStrLn |> wait);
-- |   -- send a string through the channel (and close it)
-- |   s |> send "Hello!" |> close
-- | ```
readApply : forall a:*T b:1S . (a -> ()) {- Consumer a -} -> ?a ; b 1-> b
readApply f c =
  let (x, c) = receive c in
  f x;
  c

-- | Receives a value from a channel that continues to `Wait`, closes the 
-- | continuation and returns the value.
-- | 
-- | ```
-- | main : ()
-- | main =
-- |   -- create channel endpoints
-- |   let (c, s) = new @(?String ; Wait) () in
-- |   -- fork a thread that prints the received value (and closes the channel)
-- |   fork (\_:() 1-> c |> receiveAndWait @String |> putStrLn);
-- |   -- send a string through the channel (and close it)
-- |   s |> send "Hello!" |> close
-- | ```
receiveAndWait : forall a:1T . ?a ; Wait -> a 
receiveAndWait c =
  let (x, c) = receive c in 
  wait c;
  x

-- | As in receiveAndWait only that the type is Wait and the function closes the
-- | channel rather the waiting for the channel to be closed.
receiveAndClose : forall a:1T . ?a ; Close -> a 
receiveAndClose c =
  let (x, c) = receive c in 
  close c;
  x

-- | Sends a value on a given channel and then waits for the channel to be
-- | closed. Returns ().
sendAndWait : forall a:1T . a -> !a ; Wait 1-> ()
sendAndWait x c = c |> send x |> wait

-- | Sends a value on a given channel and then closes the channel.
-- | Returns ().
sendAndClose : forall a:1T . a -> !a ; Close 1-> ()
sendAndClose x c = c |> send x |> close

-- | Receives a value from a star channel. Unrestricted version of `receive`.
receive_ : forall a:1T . *?a -> a
receive_ c = c |> receive |> fst @a @*?a

-- | Sends a value on a star channel. Unrestricted version of `send`.
send_ : forall a:1T . a -> *!a 1-> ()
send_ x c = c |> send x |> sink @*!a