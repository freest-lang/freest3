main : Bool
main = recvFun @Bool True

recvFun : ∀a . a -> a
recvFun =
  let (w, r) = new @(!(∀a. a -> a);Close) () in
  fork @() (\_:()1-> send (Λa => λx:a -> x) w |> close);
  receiveAndWait @(∀a . a -> a) r
