main : Bool
main = recvFun @Bool True

recvFun : ∀a . a -> a
recvFun =
  let (w, r) = new !(∀a. a -> a) in
  fork (\_:() 1-> send (Λa => λx:a -> x) w);
  let (f, _) = receive r in f
