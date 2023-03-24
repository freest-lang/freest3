f : ∀a . a -> (a, a)
f x =
  let (r, w) = new @(?a;End) () in
  fork (\_:()1-> send x w |> close);
  let y = receiveAndClose @a r in 
  (y, y)

main : (Bool, Bool)
main =
  f @Bool True
