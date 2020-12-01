type IntPred = !Int ; ?Bool

type IntPredDual = ?Int; !Bool

client : IntPred -> Bool
client c =
  let (b, _) = receive $ send 5 c in
  b

server : IntPredDual -> Skip
server s =
  let (n, s') = receive s in
  send (n >= 0) s'

main : Bool
main = 
  let (x, y) = new IntPred in -- x: IntPred, y: IntPredDual
  fork (sink (server y));
  client x

sink : Skip -> ()
sink _ = ()
