{- |
Module      :  Exchange a JSON values on a channel
Description :  As in "Context-Free Session Types", ICFP'16
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  Diogo Barros <fc51959@alunos.fc.ul.pt>

The JSON format (ECMA-404 The JSON Data Interchange Standard), is  inherently context free, and because of this, its implementation in FreeST is rather natural and fluent.

More info on json at https://www.json.org
-}

main : Object
main =
  let (w, r) = new ObjectChannel in
  fork [Skip] $ writeObject[Skip] json w;
  fst [Object, Skip] $ readObject [Skip] r

-- A dataype for JSON
data Value = StringVal String |
             IntVal    Int    |
             ObjectVal Object |
             ArrayVal  Array  |
             BoolVal   Bool   |
             NullVal
data Object = ConsObject String Value Object | EmptyObject
data Array = ConsArray Value Array | EmptyArray

-- A JSON value
json : Object
json = ConsObject "name" (StringVal "James") $
       ConsObject "age" (IntVal 30) $
       ConsObject "car" NullVal $
       ConsObject "children" (ArrayVal $
         ConsArray (ObjectVal $
           ConsObject "name" (StringVal "Jonah") EmptyObject) $
         ConsArray (ObjectVal $
           ConsObject "name" (StringVal "Johanson") EmptyObject) $
         EmptyArray) $
       EmptyObject

-- Channels for sending JSON objects
type ValueChannel : SL = +{
    StringVal : !String,
    IntVal    : !Int,
    ObjectVal : ObjectChannel,
    ArrayVal  : ArrayChannel,
    BoolVal   : !Bool,
    NullVal   : Skip
  }
type ObjectChannel : SL = +{
    ConsObject : !String; ValueChannel; ObjectChannel,
    Empty      : Skip
  }
type ArrayChannel : SL = +{
    ConsObject : ValueChannel; ArrayChannel,
    Empty      : Skip
  }

-- Writing a JSON value on a channel
writeValue : forall a : SL . Value -> ValueChannel;a -> a
writeValue (StringVal s) c = select StringVal c & send s
writeValue (IntVal    i) c = select IntVal    c & send i
writeValue (ObjectVal j) c = select ObjectVal c & writeObject [a] j
writeValue (ArrayVal  l) c = select ArrayVal  c & writeArray [a] l
writeValue (BoolVal   b) c = select BoolVal   c & send b
writeValue NullVal       c = select NullVal   c

writeObject : forall a:SL . Object -> ObjectChannel;a -> a
writeObject EmptyObject               = select Empty c
writeObject (ConsObject key val j1) c =
      select ConsObject c &
      send key &
      writeValue [ObjectChannel;a] val &
      writeObject [a] j1

writeArray : forall a:SL . Array -> ArrayChannel;a -> a
writeArray EmptyArray c       = select Empty c
writeArray (ConsArray j l1) c =
      select ConsObject c &
      writeValue [ArrayChannel;a] j &
      writeArray [a] l1

-- Reading a JSON value from a channel
readValue : forall a : SL . dualof ValueChannel;a -> (Value, a)
readValue (StringVal c) = let (s, c) = receive c in (StringVal s, c)
readValue (IntVal    c) = let (i, c) = receive c in (IntVal i, c)
readValue (ObjectVal c) = let (j, c) = readObject [a] c in (ObjectVal j, c)
readValue (ArrayVal  c) = let (l, c) = readArray  [a] c in (ArrayVal l, c)
readValue (BoolVal   c) = let (b, c) = receive c in (BoolVal b, c)
readValue (NullVal   c) = (NullVal, c)

readObject : forall a:SL . dualof ObjectChannel;a -> (Object, a)
readObject (Empty c)      = (EmptyObject, c)
readObject (ConsObject c) =
      let (key, c)   = receive c in
      let (value, c) = readValue [dualof ObjectChannel;a] c in
      let (next, c)  = readObject [a] c in
      (ConsObject key value next, c)

readArray : forall a:SL . dualof ArrayChannel;a -> (Array, a)
readArray (ConsObject c) =
      let (j, c) = readValue [dualof ArrayChannel;a] c in
      let (l, c) = readArray [a] c in
      (ConsArray j l, c)
readArray Empty c = (EmptyArray, c)