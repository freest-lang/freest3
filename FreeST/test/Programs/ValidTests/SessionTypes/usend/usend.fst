{-
Unary send.
The type of send is

  ∀a:ML . a -> ∀b:SL . !a;b 1-> b

yet it can be used with an MU type via eta-conversion.
-}

-- usend : ∀a:MU . a -> () -> ∀b:SL . !a;b 1-> b
-- usend = Λa:MU => λx:a -> λ_:() -> Λb:SL => send [a] x [b]

main : ()
main =
  -- let unfunc = usend [Int] 5 in -- unfunc : () -> ∀b: 1S . !Int;b 1-> b
  let unfunc = λ_:() -> send  @Int 5 in -- unfunc : () -> ∀b: 1S . !Int;b 1-> b
  let (s1, r1) = new !Int;End in
  let (s2, r2) = new !Int;End in
  fork @() (\_:()1-> unfunc () @End s1 |> close);
  fork @() (\_:()1-> unfunc () @End s2 |> close);
  fork @() (\_:()1-> let (x, r1) = receive r1 in close r1; printIntLn x);
  let (x, r2) = receive r2 in printIntLn x ;
  close r2
