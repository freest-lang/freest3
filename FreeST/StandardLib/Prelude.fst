module Prelude where

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
-- Pair
fst : ∀ a:1T . ∀ b:*T . (a, b) -> a
snd : ∀ a:*T . ∀ b:1T . (a, b) -> b
-- Print
printInt : Int -> ()
printIntLn : Int -> ()
printBool : Bool -> ()
printBoolLn : Bool -> ()
printChar : Char -> ()
printCharLn : Char -> ()
printUnit : () -> ()
printUnitLn : () -> ()
printString : String -> ()
printStringLn : String -> ()
  -- Fork
fork : ∀a:*T. (() 1-> a) -> ()
  -- Error & Undefined
error : ∀a:*T . String -> a
undefined : ∀a:*T . a
  -- Session ops
send : ∀a:1T . a -> ∀b:1S . !a;b 1-> b
receive : ∀a:1T . ∀b:1S . ?a;b -> (a, b)
close : End -> ()
  -- Not the actual type for collect, but for writing it we would
  -- need polymorphism over the labels in some choice/variant
collect : ∀a:*T . a

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

-- | A mark for functions that do not terminate
type Diverge = ()

-- | A function that diverges
diverge : Diverge
diverge = diverge

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
    let (x, y) = new a in
    fork (λ_:() 1-> f y);
    x

-- | Session initiation. Accept a request for a linear session on a shared
-- channel. The requester uses a conventional receive to obtain the channel end.
accept : ∀a:1S b:*S . !a; b -> dualof a
accept ch =
    let (x, y) = new a in
    send x ch;
    y

-- |Session initiation on an star channel
accept_ : ∀a:1S . *!a -> dualof a
accept_ ch = accept @a @*!a ch

-- | Run a server process given a function to serve a client (a handle), the
-- initial state and the server's endpoint.
runServer : ∀a:1S b . (b -> dualof a 1-> b) -> b -> *!a -> Diverge
runServer handle state ch =
    runServer @a @b handle (handle state (accept_ @a ch)) ch 

-- | Receive a value from a channel that continues to End, close the 
-- continuation and return the value.
receiveAndClose : ∀a:1T . ?a;End -> a 
receiveAndClose c =
    let (x, c) = receive c in 
    close c;
    x
