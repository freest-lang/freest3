{-

This code shows how to send and receive JSON data.

The JSON format (ECMA-404 The JSON Data Interchange Standard), is
  inherently context free, and because of this, its implementation in
  FreeST is rather natural and fluent.

-}


main : JSON
main =
  let (w, r) = new JSONChannel in
  let _ = fork $ sink $ sendJSON[Skip] json w in
  fst[JSON, Skip] $ receiveJSON[Skip] r

sink : Skip -> ()
sink s = ()

data JSON = ConsJSON String Value JSON | NilJSON

data JSONList = ConsJSONList JSON JSONList | NilJSONList

data Value = StringVal String     |
             IntVal Int           |
             BoolVal Bool         |
             JSONVal JSON         |
             JSONListVal JSONList |
             NilVal

-- JSON example:
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
json : JSON
json = ConsJSON "name"     (StringVal "James") $
       ConsJSON "age"      (IntVal 30) $
       ConsJSON "car"      NilVal $
       ConsJSON "children" (JSONListVal $ ConsJSONList (ConsJSON "name" (StringVal "Jonah")    NilJSON) $
                                          ConsJSONList (ConsJSON "name" (StringVal "Johanson") NilJSON) $
                                          NilJSONList) $
       NilJSON

printStringQLn : String -> ()
printStringQLn s = printString "\""; printString s; printString "\""

-- Channel for sending JSONObjects
type JSONChannel : SL = +{
    Cons: !String; ValueChannel; JSONChannel,
    Nil: Skip
  }

-- Channel for sending values
type ValueChannel : SL = +{
    StringVal   : !String,
    IntVal      : !Int,
    BoolVal     : !Bool,
    JSONVal     : JSONChannel,
    JSONListVal : JSONListChannel,
    NilVal      : Skip
  }

-- Channel for sending JSON lists
type JSONListChannel : SL = +{
    Cons: JSONChannel; JSONListChannel,
    Nil: Skip
  }



-- Sending

sendVal : forall a : SL . Value -> ValueChannel;a -> a
sendVal v c =
  case v of {
    StringVal s   -> send s $ select StringVal c,
    IntVal i      -> send i $ select IntVal c,
    BoolVal b     -> send b $ select BoolVal c,
    JSONVal j     -> sendJSON[a] j $ select JSONVal c,
    JSONListVal l -> sendJSONListVal[a] l $ select JSONListVal c,
    NilVal        ->          select NilVal c
  }

sendJSON : forall a : SL . JSON -> JSONChannel;a -> a
sendJSON j c =
  case j of {
    ConsJSON key val j1 ->
      sendJSON[a] j1 $ sendVal[JSONChannel;a] val $ send key $ select Cons c,
    NilJSON ->
      select Nil c
  }

sendJSONListVal : forall a : SL . JSONList -> JSONListChannel;a -> a
sendJSONListVal l c =
  case l of {
    ConsJSONList j l1 ->
      sendJSONListVal[a] l1 $ sendJSON[JSONListChannel;a] j $ select Cons c,
    NilJSONList ->
      select Nil c
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
    BoolVal c ->
      let (b, c) = receive c in
      (BoolVal b, c),
    JSONVal c ->
      let (j, c) = receiveJSON[a] c in
      (JSONVal j, c),
    JSONListVal c ->
      let (l, c) = receiveJSONList[a] c in
      (JSONListVal l, c),
    NilVal c ->
      (NilVal, c)
  }

receiveJSONList : forall a : SL . dualof JSONListChannel;a -> (JSONList, a)
receiveJSONList c =
  match c with {
    Cons c ->
      let (j, c) = receiveJSON[dualof JSONListChannel;a] c in
      let (l, c) = receiveJSONList[a] c in
      (ConsJSONList j l, c),
    Nil c ->
      (NilJSONList, c)
  }

receiveJSON : forall a : SL . dualof JSONChannel;a -> (JSON, a)
receiveJSON c =
  match c with {
    Cons c ->
      let (key, c) = receive c in
      let (value, c) = receiveValue[dualof JSONChannel;a] c in
      let (next, c) = receiveJSON[a] c in
      (ConsJSON key value next, c),
    Nil c ->
      (NilJSON, c)
  }
