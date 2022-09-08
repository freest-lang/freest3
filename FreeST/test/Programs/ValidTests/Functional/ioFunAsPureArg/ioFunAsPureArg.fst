main : Bool
main =
  let (s, r) = new !Int;End in
  fork @() (\_:()1-> sender s 5);
  if (div (f r) 2) == 5 then
    True
  else
    False

f : ?Int;End -> Int
f c = let (x, c) = receive c in close c; x

sender : !Int;End 1-> Int -> ()
sender c i = send (i * 2) c & close
