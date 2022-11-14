{-
Unary send.
The type of send is

  ∀a:1T . a -> ∀b:1S . !a;b 1-> b

yet it can be used with an *T type via eta-conversion.

usend : ∀a:1T . a -> ∀b:1S . !a;b 1-> b
usend = Λa:1T => λx:a -> λ_:() -> Λb:1S => send @a x @b
Error:
   The initial context is [x:a]
     the final context is []
-}

main : Int
main =
  --  unfunc : () -> ∀b:1S . !Int;b 1-> b
  let unfunc = λ_:() -> send @Int 5 in 
  let (s1, r1) = new @!Int;End () in
  let (s2, r2) = new @!Int;End () in
  fork (\_:() 1-> unfunc () @End s1 |> close);
  fork (\_:() 1-> unfunc () @End s2 |> close);
  fork (\_:() 1-> {- printIntLn $ -} receiveAndClose @Int r1);
  {- printIntLn $ -} receiveAndClose @Int r2
  
