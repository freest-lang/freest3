main : Bool
main =
  let (s, r) = new !Int in
  let _ = fork (sink (sender s 5)) in
  if (div (f r) 2) == 5 then
    True
  else
    False

f : ?Int -> Int
f c = let (x, c) = receive c in x

sender : !Int -> Int -> Skip
sender c i = send (i * 2) c

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
