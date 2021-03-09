{-

This code shows how to send and receive JSON data.

The JSON format (ECMA-404 The JSON Data Interchange Standard), is
  inherently context free, and because of this, its implementation in
  FreeST is rather natural and fluent.

More info at https://www.json.org

-}


main : Object
main =
  let (w, r) = new ObjectChannel in
  let _ = fork $ sink $ sendObject[Skip] json w in
  fst[Object, Skip] $ receiveObject[Skip] r

sink : Skip -> ()
sink s = ()

data Object = ConsObject String Value Object | EmptyObject

data Array = ConsArray Value Array | EmptyArray

data Value = StringVal String |
             IntVal    Int    |
             ObjectVal Object |
             ArrayVal  Array  |
             BoolVal   Bool   |
             NullVal

-- JSON object example:
-- {
--   "name":"James",
--   "age":30,
--   "car":null
--   "children":
--     [
--       {"name": "Jonah"},
--       {"name": "Johanson"}
--     ]
-- }
json : Object
json = ConsObject "name"     (StringVal "James") $
       ConsObject "age"      (IntVal 30) $
       ConsObject "car"      NullVal $
       ConsObject "children" (ArrayVal $ ConsArray (ObjectVal $ ConsObject "name" (StringVal "Jonah")    EmptyObject) $
                                         ConsArray (ObjectVal $ ConsObject "name" (StringVal "Johanson") EmptyObject) $
                                         EmptyArray) $
       EmptyObject


-- Channel for sending JSONObjects
type ObjectChannel : SL = +{
    Cons : !String; ValueChannel; ObjectChannel,
    Empty: Skip
  }

-- Channel for sending values
type ValueChannel : SL = +{
    StringVal : !String,
    IntVal    : !Int,
    ObjectVal : ObjectChannel,
    ArrayVal  : ArrayChannel,
    BoolVal   : !Bool,
    NullVal   : Skip
  }

-- Channel for sending JSON lists
type ArrayChannel : SL = +{
    Cons : ValueChannel; ArrayChannel,
    Empty: Skip
  }


-- Sending

sendValue : forall a : SL . Value -> ValueChannel;a -> a
sendValue v c =
  case v of {
    StringVal s -> send s $ select StringVal c,
    IntVal i    -> send i $ select IntVal c,
    ObjectVal j -> sendObject[a] j $ select ObjectVal c,
    ArrayVal l  -> sendArray[a] l $ select ArrayVal c,
    BoolVal b   -> send b $ select BoolVal c,
    NullVal     -> select NullVal c
  }

sendObject : forall a : SL . Object -> ObjectChannel;a -> a
sendObject j c =
  case j of {
    ConsObject key val j1 ->
      sendObject[a] j1 $ sendValue[ObjectChannel;a] val $ send key $ select Cons c,
    EmptyObject ->
      select Empty c
  }

sendArray : forall a : SL . Array -> ArrayChannel;a -> a
sendArray l c =
  case l of {
    ConsArray j l1 ->
      sendArray[a] l1 $ sendValue[ArrayChannel;a] j $ select Cons c,
    EmptyArray ->
      select Empty c
  }


-- Receiving

receiveValue : forall a : SL . dualof ValueChannel;a -> (Value, a)
receiveValue c =
  match c with {
    StringVal c ->
      let (s, c) = receive c in
      (StringVal s, c),
    IntVal c ->
      let (i, c) = receive c in
      (IntVal i, c),
    ObjectVal c ->
      let (j, c) = receiveObject[a] c in
      (ObjectVal j, c),
    ArrayVal c ->
      let (l, c) = receiveArray[a] c in
      (ArrayVal l, c),
    BoolVal c ->
      let (b, c) = receive c in
      (BoolVal b, c),
    NullVal c ->
      (NullVal, c)
  }

receiveArray : forall a : SL . dualof ArrayChannel;a -> (Array, a)
receiveArray c =
  match c with {
    Cons c ->
      let (j, c) = receiveValue[dualof ArrayChannel;a] c in
      let (l, c) = receiveArray[a] c in
      (ConsArray j l, c),
    Empty c ->
      (EmptyArray, c)
  }

receiveObject : forall a : SL . dualof ObjectChannel;a -> (Object, a)
receiveObject c =
  match c with {
    Cons c ->
      let (key, c)   = receive c in
      let (value, c) = receiveValue[dualof ObjectChannel;a] c in
      let (next, c)  = receiveObject[a] c in
      (ConsObject key value next, c),
    Empty c ->
      (EmptyObject, c)
  }
