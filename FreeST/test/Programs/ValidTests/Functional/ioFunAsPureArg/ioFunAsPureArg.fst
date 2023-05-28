main : Bool
main =
  let (s, r) = new @(!Int;EndC) () in
  fork @() (\_:()1-> sender s 5);
  (div (f r) 2) == 5

f : ?Int;EndW -> Int
f c = let (x, c) = receive c in wait c; x

sender : !Int;EndC -> Int 1-> ()
sender c i = send (i * 2) c |> close

