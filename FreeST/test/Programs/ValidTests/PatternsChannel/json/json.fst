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
  let (w, r) = new @(ObjectChannel;Close) () in
  fork (\_:() 1-> writeObject @Close json w |> close);
  let (obj, r) = readObject @Wait r in
  wait r;
  obj

-- A dataype for JSON
data Value = StringVal String |
             IntVal    Int    |
             ObjectVal Object |
             ArrayVal  Array  |
             BoolVal   Bool   |
             NullVal
data Object = ConsObject String Value Object | EmptyObject
data Array  = ConsArray Value Array | EmptyArray

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
type ValueChannel : 1S = +{
    StringValC : !String,
    IntValC    : !Int,
    ObjectValC : ObjectChannel,
    ArrayValC  : ArrayChannel,
    BoolValC   : !Bool,
    NullValC   : Skip
  }
type ObjectChannel : 1S = +{
    ConsObjectC : !String; ValueChannel; ObjectChannel,
    EmptyC      : Skip
  }
type ArrayChannel : 1S = +{
    ConsObjectC : ValueChannel; ArrayChannel,
    EmptyC      : Skip
  }

-- Writing a JSON value on a channel
writeValue : forall a : 1S . Value -> ValueChannel;a -> a
writeValue (StringVal s) c = select StringValC c |> send s
writeValue (IntVal    i) c = select IntValC    c |> send i
writeValue (ObjectVal j) c = select ObjectValC c |> writeObject @a j
writeValue (ArrayVal  l) c = select ArrayValC  c |> writeArray @a l
writeValue (BoolVal   b) c = select BoolValC   c |> send b
writeValue NullVal       c = select NullValC   c

writeObject : forall a:1S . Object -> ObjectChannel;a -> a
writeObject EmptyObject             c = select EmptyC c
writeObject (ConsObject key val j1) c =
      select ConsObjectC c |>
      send key |>
      writeValue @(ObjectChannel;a) val |>
      writeObject @a j1

writeArray : forall a:1S . Array -> ArrayChannel;a -> a
writeArray EmptyArray c       = select EmptyC c
writeArray (ConsArray j l1) c =
      select ConsObjectC c |>
      writeValue @(ArrayChannel;a) j |>
      writeArray @a l1

-- Reading a JSON value from a channel
readValue : forall a : 1S . dualof ValueChannel;a -> (Value, a)
readValue (StringValC c) = let (s, c) = receive c in (StringVal s, c)
readValue (IntValC    c) = let (i, c) = receive c in (IntVal i, c)
readValue (ObjectValC c) = let (j, c) = readObject @a c in (ObjectVal j, c)
readValue (ArrayValC  c) = let (l, c) = readArray  @a c in (ArrayVal l, c)
readValue (BoolValC   c) = let (b, c) = receive c in (BoolVal b, c)
readValue (NullValC   c) = (NullVal, c)

readObject : forall a:1S . dualof ObjectChannel;a -> (Object, a)
readObject (EmptyC c)      = (EmptyObject, c)
readObject (ConsObjectC c) =
      let (key, c)   = receive c in
      let (value, c) = readValue @(dualof ObjectChannel;a) c in
      let (next, c)  = readObject @a c in
      (ConsObject key value next, c)

readArray : forall a:1S . dualof ArrayChannel;a -> (Array, a)
readArray (EmptyC c)      = (EmptyArray, c)
readArray (ConsObjectC c) =
      let (j, c) = readValue @(dualof ArrayChannel;a) c in
      let (l, c) = readArray @a c in
      (ConsArray j l, c)
