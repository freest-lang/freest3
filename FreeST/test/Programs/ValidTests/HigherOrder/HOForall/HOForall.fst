main : Bool
main = recvFun @Bool True

recvFun : ∀a . a -> a
recvFun =
  let (w, r) = new @(!(∀a. a -> a);EndC) () in
  fork @() (\_:()1-> send (Λa => λx:a -> x) w |> close);
  receiveAndWait @(∀a . a -> a) r
