{- |
Module      :  Exchange a JSON values on a channel
Description :  As in "Context-Free Session Types", ICFP'16
Copyright   :  (c) LASIGE and University of Lisbon, Portugal
Maintainer  :  Diogo Barros <fc51959@alunos.fc.ul.pt>

The JSON format (ECMA-404 The JSON Data Interchange Standard), is  inherently context free, and because of this, its implementation in FreeST is rather natural and fluent.

More info on json at https://www.json.org
-}

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
type ValueChannel : 1S = +{
    StringVal : !String,
    IntVal    : !Int,
    ObjectVal : ObjectChannel,
    ArrayVal  : ArrayChannel,
    BoolVal   : !Bool,
    NullVal   : Skip
  }
type ObjectChannel : 1S = +{
    ConsObject : !String; ValueChannel; ObjectChannel,
    Empty      : Skip
  }
type ArrayChannel : 1S = +{
    ConsObject : ValueChannel; ArrayChannel,
    Empty      : Skip
  }

-- Writing a JSON value on a channel
writeValue : forall a : 1S . Value -> ValueChannel;a -> a
writeValue v c =
  case v of {
    StringVal s -> select StringVal c |> send s,
    IntVal    i -> select IntVal    c |> send i,
    ObjectVal j -> select ObjectVal c |> writeObject  @a j,
    ArrayVal  l -> select ArrayVal  c |> writeArray  @a l,
    BoolVal   b -> select BoolVal   c |> send b,
    NullVal     -> select NullVal   c
  }

and writeObject : forall a: 1S . Object -> ObjectChannel;a -> a
writeObject j c =
  case j of {
    ConsObject key val j1 ->
      select ConsObject c
      |> send key
      |> writeValue  @(ObjectChannel ; a) val
      |> writeObject  @a j1,
    EmptyObject ->
      select Empty c
  }

and writeArray : forall a: 1S . Array -> ArrayChannel;a -> a 
writeArray l c =
  case l of {
    ConsArray j l1 ->
      select ConsObject c
      |> writeValue  @(ArrayChannel ; a) j
      |> writeArray  @a l1 ,
    EmptyArray ->
      select Empty c
  }

-- Reading a JSON value from a channel
readValue  : forall a : 1S . dualof ValueChannel;a -> (Value, a)
readValue c =
  match c with {
    StringVal c -> let (s, c) = receive c in (StringVal s, c),
    IntVal    c -> let (i, c) = receive c in (IntVal i, c),
    ObjectVal c -> let (j, c) = readObject  @a c in (ObjectVal j, c),
    ArrayVal  c -> let (l, c) = readArray  @a c in (ArrayVal l, c),
    BoolVal   c -> let (b, c) = receive c in (BoolVal b, c),
    NullVal   c -> (NullVal, c)
  }

and readObject : forall a: 1S . dualof ObjectChannel;a -> (Object, a)
readObject c =
  match c with {
    ConsObject c ->
      let (key, c)   = receive c in
      let (value, c) = readValue  @(dualof ObjectChannel ; a) c in
      let (next, c)  = readObject  @a c in
      (ConsObject key value next, c),
    Empty c ->
      (EmptyObject, c)
  }

and readArray  : forall a: 1S . dualof ArrayChannel;a -> (Array, a)
readArray c =
  match c with {
    ConsObject c ->
      let (j, c) = readValue  @(dualof ArrayChannel ; a) c in
      let (l, c) = readArray  @a c in
      (ConsArray j l, c),
    Empty c ->
      (EmptyArray, c)
  }

main : Object 
main =
  let (w, r) = new @(ObjectChannel;Close) () in
  fork (\_:() 1-> writeObject @Close json w |> close);
  let (obj, r) = readObject @Wait r in
  wait r;
  obj