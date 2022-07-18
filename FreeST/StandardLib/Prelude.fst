module Prelude where

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

------------------------------------------------------------

-- stdout : OutStreamProvider
-- stdout : fst @OutStreamProvider @dualof OutStreamProvider $ new OutStreamProvider --error @OutStreamProvider "Called!"

#runStdout  : dualof OutStreamProvider -> ()
#runStdout =
    runServer @OutStream @() #runPrinter ()

#runPrinter : () -> dualof OutStream 1-> ()
#runPrinter _ printer =
    match printer with {
        PutBool     printer -> #aux @Bool   printer #printBool     & #runPrinter (),
        PutBoolLn   printer -> #aux @Bool   printer #printBoolLn   & #runPrinter (),
        PutInt      printer -> #aux @Int    printer #printInt      & #runPrinter (),
        PutIntLn    printer -> #aux @Int    printer #printIntLn    & #runPrinter (),
        PutChar     printer -> #aux @Char   printer #printChar     & #runPrinter (),
        PutCharLn   printer -> #aux @Char   printer #printCharLn   & #runPrinter (),
        PutString   printer -> #aux @String printer #printString   & #runPrinter (),
        PutStringLn printer -> #aux @String printer #printStringLn & #runPrinter (),
        Close         _       -> ()
    }

#aux : forall a . ?a;dualof OutStream -> (a -> ()) 1-> dualof OutStream
#aux printer printFun =
    let (x, printer) = receive printer in
    printFun x;
    printer

type OutStreamProvider : *S = *?OutStream
type OutStream : 1S = +{ PutBool    : !Bool  ; OutStream
                       , PutBoolLn  : !Bool  ; OutStream
                       , PutInt     : !Int   ; OutStream
                       , PutIntLn   : !Int   ; OutStream
                       , PutChar    : !Char  ; OutStream
                       , PutCharLn  : !Char  ; OutStream
                       , PutString  : !String; OutStream
                       , PutStringLn: !String; OutStream
                       , Close      : Skip
                       }

putGeneric : forall a . (OutStream -> !a;OutStream) -> OutStreamProvider -> a -> ()
putGeneric sel outProv x =                                      
    sel (receive_ @OutStream outProv) & send x & select Close & sink @Skip



putBool : OutStreamProvider -> Bool -> ()
putBool = putGeneric @Bool (\out:OutStream -> select PutBool out) 

putBoolLn : OutStreamProvider -> Bool -> ()
putBoolLn = putGeneric @Bool (\out:OutStream -> select PutBoolLn out)


putInt : OutStreamProvider -> Int -> ()
putInt = putGeneric @Int (\out:OutStream -> select PutInt out)

putIntLn : OutStreamProvider -> Int -> ()
putIntLn = putGeneric @Int (\out:OutStream -> select PutIntLn out)


putChar : OutStreamProvider -> Char -> ()
putChar = putGeneric @Char (\out:OutStream -> select PutChar out)

putCharLn : OutStreamProvider -> Char -> ()
putCharLn = putGeneric @Char (\out:OutStream -> select PutCharLn out)


putString : OutStreamProvider -> String -> ()
putString = putGeneric @String (\out:OutStream -> select PutString out)

putStringLn : OutStreamProvider -> String -> ()
putStringLn = putGeneric @String (\out:OutStream -> select PutStringLn out)



printBool : Bool -> ()
printBool = putBool stdout

printBoolLn : Bool -> ()
printBoolLn = putBoolLn stdout


printInt : Int -> ()
printInt = putInt stdout

printIntLn : Int -> ()
printIntLn = putIntLn stdout


printChar : Char -> ()
printChar = putChar stdout

printCharLn : Char -> ()
printCharLn = putCharLn stdout


printString : String -> ()
printString = putString stdout

printStringLn : String -> ()
printStringLn = putStringLn stdout

------------------------------------------------------------

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