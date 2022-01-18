{-
Unary send.
The type of send is

  ∀a:ML . a -> ∀b:SL . !a;b -o b

(notice the un-arrow type, -o)
yet it can be used with an MU type via eta-conversion.
-}

usend : ∀a:MU . a -> () -> ∀b:SL . !a;b -o b
usend = Λa:MU => λx:a -> λ_:() -> Λb:SL => send [a] x [b]

main : (Int, Skip)
main =
  let (s, r) = new !Int in
  fork $ usend [Int] 5 () [Skip] s;
  receive r
