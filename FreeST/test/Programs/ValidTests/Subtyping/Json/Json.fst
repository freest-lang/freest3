-- | Representation of JSON values
data Json = JObject JObject
          | JArray  JArray 
          | JString String 
          | JNumber Int 
          | JBool   Bool 
          | JNull
data JObject = JONil | JOCons String Json JObject
data JArray  = JANil | JACons        Json JArray

-- | Serialization protocol for JSON values
type JsonSender = 
  +{ SendJObject: JObjectSender
   , SendJArray : JArraySender
   , SendJString: !String
   , SendJNumber: !Int
   , SendJBool  : !Bool
   , SendJNull  : Skip
   }
type JsonReceiver = dualof JsonSender

type JObjectSender = 
  +{ SendJONil : Skip
   , SendJOCons: !String;JsonSender;JObjectSender
   }
type JObjectReceiver = dualof JObjectSender

type JArraySender = 
  +{ SendJANil : Skip
   , SendJACons: JsonSender;JArraySender
   }
type JArrayReceiver = dualof JArraySender 

-- | Deserializes a JSON value, returning the value and the continuation of the channel.
deserializeJson : JsonReceiver;a -> (Json, a)
deserializeJson (SendJObject c) = let (o, c) = deserializeJObject @a c in (JObject o, c)
deserializeJson (SendJArray  c) = let (a, c) = deserializeJArray  @a c in (JArray  a, c)
deserializeJson (SendJString c) = let (s, c) = receive c in (JString s, c)
deserializeJson (SendJNumber c) = let (n, c) = receive c in (JNumber n, c)
deserializeJson (SendJBool   c) = let (b, c) = receive c in (JBool   b, c)
deserializeJson (SendJNull   c) = (JNull, c)
-- | where... 
-- | Deserializes a JSON object, returning the object and the continuation of the channel.
and deserializeJObject : JObjectReceiver;a -> (JObject, a)
deserializeJObject (SendJOCons c) = let (s , c) = receive c in 
                                    let (j , c) = deserializeJson @(JObjectReceiver;a) c in 
                                    let (js, c) = deserializeJObject @a c in
                                    (JOCons s j js, c)
deserializeJObject (SendJONil  c) = (JONil, c) 
-- | where...
-- | Deserializes a JSON array, returning the array and the continuation of the channel.
and deserializeJArray : JArrayReceiver;a -> (JArray, a)
deserializeJArray (SendJACons c) = let (j , c) = deserializeJson @(JArrayReceiver;a) c in 
                                   let (js, c) = deserializeJArray @a c in 
                                   (JACons j js, c)
deserializeJArray (SendJANil  c) = (JANil, c) 


-- | Serialization protocol for JSON number arrays. 
-- | Subtyping guarantees compatibility with the general JSON serialization protocol.
type NumberArraySender  = +{ SendJArray : NumberArraySender' }
type NumberSender       = +{ SendJNumber: !Int }
type NumberArraySender' = +{ SendJACons : NumberSender;NumberArraySender'
                           , SendJANil  : Skip 
                           }

-- | Traverses the list, serializing each integer in order
sendNumberArray : [Int] -> NumberArraySender';a -> a
sendNumberArray []      c = c |> select SendJANil
sendNumberArray (n::ns) c = c |> select SendJACons 
                              |> select SendJNumber 
                              |> send n 
                              |> sendNumberArray @a ns

-- | Serializes a list of integers as a JSON number array
serializeIntList : [Int] -> NumberArraySender;a -> a
serializeIntList l c = c |> select SendJArray |> sendNumberArray @a l

-- | Serializes a list of integers as a JSON value.
-- | Notice that we create a JsonSender;a channel capable of serializing any JSON value, 
-- | and pass this channel to serializeIntList, which expects a NumberArraySender;a channel,
-- | which can only serialize JSON number arrays.
-- | This is no problem, as a channel capable of serializing general JSON values is also 
-- | capable of serializing JSON arrays of numbers. Subtyping gives the type system the
-- | flexibility to allow this:  JsonSender;a <: NumberArraySender;a.
main : Json 
main = let (o,i) = new @(JsonSender;Close) () in 
       fork (\_:() 1-> serializeIntList @Close [1,2,3,4] o |> close);
       let (j, i) = deserializeJson @Wait i in 
       wait i; j 