f : a -> (a, a)
f x =
  let (r, w) = new @(?a;Wait) () in
  fork (\_:()1-> send x w |> close);
  let y = receiveAndWait @a r in 
  (y, y)

main : (Bool, Bool)
main =
  f @Bool True
