-- This program does not terminate

client : Int -> rec x:SU.(!Int;x;!Bool) -> Char
client n c =
  let c = send c n in 
  client (n+1) c

server : rec x:SU.(?Int;x;!Char;?Bool) -> Char
server c =
  let n, c = receive c in
  server c

main : Char
main =
  let w, r = new rec x:SU.(!Int;x) in
  let t = fork (client 0 w) in
  server r

