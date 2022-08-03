{-

Prelude structure:
    1. Util       - utility functions
    2. IO         - input/output functions (including files)
    3. Concurrent - functions for concurrency and shared channels

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
-- $$  __$$\ 
-- \__/  $$ |
--  $$$$$$  |
-- $$  ____/ 
-- $$ |      
-- $$$$$$$$\ 
-- \________|



-- | OutStream

type OutStreamProvider : *S = *?OutStream
type OutStream : 1S = +{ PutString  : !String; OutStream
                       , PutStringLn: !String; OutStream
                       , Close      : Skip
                       }


#genericHPut : forall a b:1S . (+{PutString: !String; b, PutStringLn: !String; b, Close: Skip} -> !a;b) -> a -> +{PutString: !String; b, PutStringLn: !String; b, Close: Skip} -> b
#genericHPut sel x outStream = sel outStream & send x

hPutString, hPutStringLn : String -> OutStream -> OutStream
hPutString   = #genericHPut @String @OutStream (\out:OutStream -> select PutString out)
hPutStringLn = #genericHPut @String @OutStream (\out:OutStream -> select PutStringLn out)


#genericPut : forall a . (OutStream -> !a;OutStream) -> a -> OutStreamProvider -> ()
#genericPut sel x outProv = 
    sink @Skip $ select Close $ #genericHPut @a @OutStream sel x $ receive_ @OutStream outProv 

putString, putStringLn : String -> OutStreamProvider -> ()
putString   = #genericPut @String (\out:OutStream -> select PutString out)
putStringLn = #genericPut @String (\out:OutStream -> select PutStringLn out)

-- | InStream

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

printString, printStringLn : String -> ()
printString   = flip @String @OutStreamProvider @() putString stdout
printStringLn = flip @String @OutStreamProvider @() putStringLn stdout

-- Internal stdout functions
#runStdout  : dualof OutStreamProvider -> ()
#runStdout =
    runServer @OutStream @() #runPrinter ()

#runPrinter : () -> dualof OutStream 1-> ()
#runPrinter _ printer =
    match printer with {
        PutString   printer -> receiveAnd @String @dualof OutStream (#printValue @String) printer & #runPrinter (),
        PutStringLn printer -> receiveAnd @String @dualof OutStream (\s:String -> #printValue @String (s ++ "\n")) printer & #runPrinter (),
        Close         _       -> ()
    }

-- | Stderr

-- Internal stderr functions
#runStderr  : dualof OutStreamProvider -> ()
#runStderr =
    runServer @OutStream @() #runErrPrinter ()

#runErrPrinter : () -> dualof OutStream 1-> ()
#runErrPrinter _ printer =
    match printer with {
        PutString   printer -> receiveAnd @String @dualof OutStream (#printErrValue @String) printer & #runErrPrinter (),
        PutStringLn printer -> receiveAnd @String @dualof OutStream (\s:String -> #printErrValue @String (s ++ "\n")) printer & #runErrPrinter (),
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
#runStdin : dualof InStreamProvider -> ()
#runStdin =
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


-- | Files

-- type FailableR a: 1S = &{Ok: ?a, Nok: Skip}
type FailableR : 1S = &{Ok: ReadFileStream , Error: Skip}
type FailableW : 1S = &{Ok: WriteFileStream, Error: Skip}

type ReadFileStream  : 1S = +{ GetBool  : &{Just: ?Bool  , Nothing: Skip}; FailableR {-Failable @ReadFileStream-}
                             , GetInt   : &{Just: ?Int   , Nothing: Skip}; FailableR {-Failable @ReadFileStream-}
                             , GetChar  : &{Just: ?Char  , Nothing: Skip}; FailableR {-Failable @ReadFileStream-}
                             , GetString: &{Just: ?String, Nothing: Skip}; FailableR {-Failable @ReadFileStream-}
                             , Close    : Skip
                             }

type WriteFileStream : 1S = +{ PutString  : !String; FailableW {-Failable @WriteFileStream-}
                             , PutStringLn: !String; FailableW {-Failable @WriteFileStream-}
                             , Close      : Skip
                             }

hFilePutString, hFilePutStringLn : String -> WriteFileStream -> FailableW
hFilePutString   = #genericHPut @String @FailableW (\out:WriteFileStream -> select PutString   out)
hFilePutStringLn = #genericHPut @String @FailableW (\out:WriteFileStream -> select PutStringLn out)




type FilePath = String
data FileHandle = FileHandle () 

data MaybeF  = JustF ((WriteFileStream, dualof WriteFileStream), FileHandle) | NothingF 
data FailedF = Ok | Error
data MaybeS   = JustS  WriteFileStream | NothingS
data MaybeSS  = JustSS WriteFileStream | NothingSS


openWriteFile : FilePath -> MaybeS
openWriteFile fp = 
    case #openWriteFile @FileHandle @WriteFileStream @MaybeF fp FileHandle (JustF, NothingF) of {
        JustF v -> 
            let (chs, fh) = v in
            let (c, s) = chs in
            fork $ runWriteFileStream fh s;
            JustS c,
        NothingF -> NothingS
    }



runWriteFileStream : FileHandle -> dualof WriteFileStream -> ()
runWriteFileStream fh ch =
    match ch with {
        PutString   ch -> let (s, ch) = receive ch in #f s fh ch,
        PutStringLn ch -> let (s, ch) = receive ch in #f (s++"\n") fh ch,
        Close       _  -> #closeFile @FileHandle fh
    }

#f : String -> FileHandle -> dualof FailableW -> ()
#f s fh ch =
    case #putFile @FileHandle @FailedF s fh (Ok, Error) of {
        Ok    -> select Ok    ch & runWriteFileStream fh,
        Error -> select Error ch & sink @Skip
    }
    -- runWriteFileStream fh $ select Ok ch


openReadFile : FilePath -> MaybeS
openReadFile fp = 
    case #openReadFile @FileHandle @WriteFileStream @MaybeF fp FileHandle (JustF, NothingF) of {
        JustF v -> 
            let (chs, fh) = v in
            let (c, s) = chs in
            fork $ runWriteFileStream fh s;
            JustS c,
        NothingF -> NothingS
    }







--  $$$$$$\  
-- $$ ___$$\ 
-- \_/   $$ |
--   $$$$$ / 
--   \___$$\ 
-- $$\   $$ |
-- \$$$$$$  |
--  \______/ 



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
receiveAnd : forall a b:1S . (a -> ()) {- Consumer a -} -> ?a;b 1-> b
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