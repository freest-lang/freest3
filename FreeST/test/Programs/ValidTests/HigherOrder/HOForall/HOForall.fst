main : Bool
main = recvFun @Bool True

recvFun : ∀a . a -> a
recvFun =
  let (w, r) = new !(∀a. a -> a);End in
  fork @() (\_:()1-> send (Λa => λx:a -> x) w & close);
  let (f, r) = receive r in close r; f
