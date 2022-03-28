{-
Unary send.
The type of send is

  ∀a:ML . a -> ∀b:SL . !a;b -o b

yet it can be used with an MU type via eta-conversion.
-}

-- usend : ∀a:MU . a -> () -> ∀b:SL . !a;b -o b
-- usend = Λa:MU => λx:a -> λ_:() -> Λb:SL => send [a] x [b]

main : ()
main =
  -- let unfunc = usend [Int] 5 in -- unfunc : () -> ∀b:SL . !Int;b -o b
  let unfunc = λ_:() -> send [Int] 5 in -- unfunc : () -> ∀b:SL . !Int;b -o b
  let (s1, r1) = new !Int in
  let (s2, r2) = new !Int in
  fork $ unfunc () [Skip] s1;
  fork $ unfunc () [Skip] s2;
  fork (let (x, _) = receive r1 in printIntLn x);
  let (x, _) = receive r2 in printIntLn x ;
  ()
