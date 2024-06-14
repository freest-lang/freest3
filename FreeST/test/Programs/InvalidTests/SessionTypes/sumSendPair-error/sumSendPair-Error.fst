-- TEST ERROR MESSAGES

type Value = Int
type Triple = (!Int;Close, (Value, Value))
type Pair = (!Int;Close, Value)

pairToValue : Pair -> Value
pairToValue p =
  let (x, y) = p in x+y

sendValue : Triple -> ()
sendValue t =
  let (c, pair) = t in
  send c (pairToValue pair) |> close
  

rcvValue : ?Int -> Value
rcvValue c = let (v, c) = receive c in v
  
 
main : Value
main =
  let (x, y) = new @(!Int;Close) () in   
  let aTriple = (x, (2, 3)) in
  let _ = fork @() (\_:() 1-> sendValue aTriple) in
  let (x, c) = receive y in wait c; x     
