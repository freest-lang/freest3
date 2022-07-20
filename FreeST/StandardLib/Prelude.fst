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


-- | OutStream & InStream

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

#putGeneric : forall a . (OutStream -> !a;OutStream) -> OutStreamProvider -> a -> ()
#putGeneric sel outProv x =                                      
    sel (receive_ @OutStream outProv) & send x & select Close & sink @Skip

putBool : OutStreamProvider -> Bool -> ()
putBool = #putGeneric @Bool (\out:OutStream -> select PutBool out) 

putBoolLn : OutStreamProvider -> Bool -> ()
putBoolLn = #putGeneric @Bool (\out:OutStream -> select PutBoolLn out)


putInt : OutStreamProvider -> Int -> ()
putInt = #putGeneric @Int (\out:OutStream -> select PutInt out)

putIntLn : OutStreamProvider -> Int -> ()
putIntLn = #putGeneric @Int (\out:OutStream -> select PutIntLn out)


putChar : OutStreamProvider -> Char -> ()
putChar = #putGeneric @Char (\out:OutStream -> select PutChar out)

putCharLn : OutStreamProvider -> Char -> ()
putCharLn = #putGeneric @Char (\out:OutStream -> select PutCharLn out)


putString : OutStreamProvider -> String -> ()
putString = #putGeneric @String (\out:OutStream -> select PutString out)

putStringLn : OutStreamProvider -> String -> ()
putStringLn = #putGeneric @String (\out:OutStream -> select PutStringLn out)


type InStreamProvider : *S = *?InStream
type InStream : 1S = +{ GetBool  : &{Just: ?Bool  , Nothing: Skip}; InStream
                      , GetInt   : &{Just: ?Int   , Nothing: Skip}; InStream
                      , GetChar  : &{Just: ?Char  , Nothing: Skip}; InStream
                      , GetString: &{Just: ?String, Nothing: Skip}; InStream
                      , Close     : Skip
                      }

data MaybeBool   = NothingBool   | JustBool   Bool
data MaybeInt    = NothingInt    | JustInt    Int
data MaybeChar   = NothingChar   | JustChar   Char
data MaybeString = NothingString | JustString String

#genericGet : forall a b . (InStream -> &{Just: ?a  , Nothing: Skip};InStream) -> (a -> b, b) -> InStreamProvider -> b
#genericGet sel maybeCons insp =
    let (justCons, nothingCons) = maybeCons in
    let ins = sel $ receive_ @InStream insp in
    let (maybe, ins) = 
        match ins with {
            Just    ins -> 
                let (b, ins) = receive ins in
                (justCons b, ins),
            Nothing ins -> 
                (nothingCons, ins)
        }
        in
    let _ = select Close ins in
    maybe

getBool : InStreamProvider -> MaybeBool
getBool =
    #genericGet @Bool @MaybeBool
        (\inS:InStream -> select GetBool inS)
        (JustBool, NothingBool)

getInt : InStreamProvider -> MaybeInt
getInt =
    #genericGet @Int @MaybeInt
        (\inS:InStream -> select GetInt inS)
        (JustInt, NothingInt)

getChar : InStreamProvider -> MaybeChar
getChar =
    #genericGet @Char @MaybeChar
        (\inS:InStream -> select GetChar inS)
        (JustChar, NothingChar)

getString : InStreamProvider -> MaybeString
getString =
    #genericGet @String @MaybeString
        (\inS:InStream -> select GetString inS)
        (JustString, NothingString)

-- | Stdout

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

-- Internal stdout functions
#runStdout  : dualof OutStreamProvider -> ()
#runStdout =
    runServer @OutStream @() #runPrinter ()

#runPrinter : () -> dualof OutStream 1-> ()
#runPrinter _ printer =
    match printer with {
        PutBool     printer -> receiveAnd @Bool   @dualof OutStream (#printValue   @Bool  ) printer & #runPrinter (),
        PutBoolLn   printer -> receiveAnd @Bool   @dualof OutStream (#printValueLn @Bool  ) printer & #runPrinter (),
        PutInt      printer -> receiveAnd @Int    @dualof OutStream (#printValue   @Int   ) printer & #runPrinter (),
        PutIntLn    printer -> receiveAnd @Int    @dualof OutStream (#printValueLn @Int   ) printer & #runPrinter (),
        PutChar     printer -> receiveAnd @Char   @dualof OutStream (#printValue   @Char  ) printer & #runPrinter (),
        PutCharLn   printer -> receiveAnd @Char   @dualof OutStream (#printValueLn @Char  ) printer & #runPrinter (),
        PutString   printer -> receiveAnd @String @dualof OutStream (#printValue   @String) printer & #runPrinter (),
        PutStringLn printer -> receiveAnd @String @dualof OutStream (#printValueLn @String) printer & #runPrinter (),
        Close         _       -> ()
    }

-- | Stdin

inputBool : MaybeBool
inputBool = getBool stdin

inputInt : MaybeInt
inputInt = getInt stdin

inputChar : MaybeChar
inputChar = getChar stdin

inputString : MaybeString
inputString = getString stdin

-- Internal stdin functions
#runStdIn : dualof InStreamProvider -> ()
#runStdIn =
    runServer @InStream @() #runReader ()

#runReader : () -> dualof InStream 1-> ()
#runReader _ reader = 
    match reader with {
        GetBool reader -> #runReader () $
            case #readBool @MaybeBool (JustBool, NothingBool) of {
                JustBool x  -> select Just reader & send x,
                NothingBool -> select Nothing reader
            },
        GetInt reader -> #runReader () $
            case #readInt @MaybeInt (JustInt, NothingInt) of {
                JustInt x  -> select Just reader & send x,
                NothingInt -> select Nothing reader
            },
        GetChar reader -> #runReader () $
            case #readChar @MaybeChar (JustChar, NothingChar) of {
                JustChar x  -> select Just reader & send x,
                NothingChar -> select Nothing reader
            },
        GetString reader -> #runReader () $
            case #readString @MaybeString (JustString, NothingString) of {
                JustString x  -> select Just reader & send x,
                NothingString -> select Nothing reader
            },
        Close _-> ()
    }

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

-- | Receive a value from a linear channel and apply a function to it.
--   Returns the continuation channel
receiveAnd : forall a b:1S . (a -> ()) -> ?a;b 1-> b
receiveAnd f ch =
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