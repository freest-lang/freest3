main : Bool
main = recvFun @Bool True

recvFun : ∀a . a -> a
recvFun =
  let (w, r) = new !(∀a. a -> a);End in
  fork $ send (Λa => λx:a -> x) w;
  let (f, r) = receive r in close r; f
