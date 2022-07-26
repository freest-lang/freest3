-- stdin session types

type StdIn  : *S = *?Reader
type Reader : 1S = +{ ReadBool  : &{Just: ?Bool  , Nothing: Skip}; Reader
                    , ReadInt   : &{Just: ?Int   , Nothing: Skip}; Reader
                    , ReadChar  : &{Just: ?Char  , Nothing: Skip}; Reader
                    , ReadString: &{Just: ?String, Nothing: Skip}; Reader
                    , Close     : Skip
                    }

{-
-- when type apps is available 
type Reader : 1S = +{ ReadBool  : MaybeC[Bool]  ; Reader
                    , ReadInt   : MaybeC[Int]   ; Reader
                    , ReadChar  : MaybeC[Char]  ; Reader
                    , ReadString: MaybeC[String]; Reader
                    , Close     : Skip
                    }

type MaybeC : 1S = forall a . &{JustC: ?a  , NothingC: Skip}
-}

-- helper functions

{-
data Maybe a = Nothing | Just a

readGeneric : forall a . (Reader -> MaybeC[a]; Reader) -> StdIn -> Maybe a
readGeneric sel stdin =
    let reader = sel $ fst[Reader, StdIn] $ stdin in
    let (maybe, reader) = 
        match reader with {
            JustC    reader -> 
                let (d, reader) = receive reader in
                (Just d, reader),
            NothingC reader -> 
                (Nothing, reader)
        }
        in
    let _ = select Close reader in
    maybe

readBool : StdIn -> Maybe[Bool]
readBool = readGeneric[Bool] (\stdin:StdIn -> select ReadBool stdin)

readInt : StdIn -> Maybe[Int]
readInt = readGeneric[Int] (\stdin:StdIn -> select ReadInt stdin)

readChar : StdIn -> Maybe[Char]
readChar = readGeneric[Char] (\stdin:StdIn -> select ReadChar stdin)

readString : StdIn -> Maybe[String]
readString = readGeneric[String] (\stdin:StdIn -> select ReadString stdin)
-}


data MaybeBool   = NothingBool   | JustBool   Bool
data MaybeInt    = NothingInt    | JustInt    Int
data MaybeChar   = NothingChar   | JustChar   Char
data MaybeString = NothingString | JustString String

readBool : StdIn -> MaybeBool
readBool stdin =
    let reader = select ReadBool $ fst @Reader @StdIn $ receive stdin in
    let (maybe, reader) = 
        match reader with {
            Just    reader -> 
                let (b, reader) = receive reader in
                (JustBool b, reader),
            Nothing reader -> 
                (NothingBool, reader)
        }
        in
    let _ = select Close reader in
    maybe

readInt : StdIn -> MaybeInt
readInt stdin =
    let reader = select ReadInt $ fst @Reader @StdIn $ receive stdin in
    let (maybe, reader) = 
        match reader with {
            Just    reader -> 
                let (i, reader) = receive reader in
                (JustInt i, reader),
            Nothing reader -> 
                (NothingInt, reader)
        }
        in
    let _ = select Close reader in
    maybe

readChar : StdIn -> MaybeChar
readChar stdin =
    let reader = select ReadChar $ fst @Reader @StdIn $ receive stdin in
    let (maybe, reader) = 
        match reader with {
            Just    reader -> 
                let (c, reader) = receive reader in
                (JustChar c, reader),
            Nothing reader -> 
                (NothingChar, reader)
        }
        in
    let _ = select Close reader in
    maybe

readString : StdIn -> MaybeString
readString stdin =
    let reader = select ReadString $ fst @Reader @StdIn $ receive stdin in
    let (maybe, reader) = 
        match reader with {
            Just    reader -> 
                let (s, reader) = receive reader in
                (JustString s, reader),
            Nothing reader -> 
                (NothingString, reader)
        }
        in
    let _ = select Close reader in
    maybe


-- stdin server

runStdIn : dualof StdIn -> ()
runStdIn stdin =
    let (c, s) = new Reader in
    let stdin  = send c stdin in
    runReader s;
    runStdIn stdin

runReader : dualof Reader -> ()
runReader reader = 
    match reader with {
        -- ReadBool   reader -> runReader $ send True    $ select Just reader, -- correct sender
        ReadBool   reader -> runReader $ select Nothing reader, -- mock error, imagine that a number was read
        ReadInt    reader -> runReader $ send 7       $ select Just reader,
        ReadChar   reader -> runReader $ send 'A'     $ select Just reader,
        ReadString reader -> runReader $ send "hello" $ select Just reader,
        Close     _      -> ()
    }


-- clients

client1 : StdIn -> ()
client1 stdin = 
    case readBool stdin of {
        JustBool b  -> printBoolLn b,
        NothingBool -> printStringLn "Oops an error ocurred!"
    }

client2 : StdIn -> ()
client2 stdin = 
    case readInt stdin of {
        JustInt b  -> printIntLn b,
        NothingInt -> printStringLn "Oops an error ocurred!"
    }


-- main

main : ()
main = 
    let (c, s) = new StdIn in
    fork (\_:() 1-> client1 c);
    fork (\_:() 1-> client2 c);
    runStdIn s