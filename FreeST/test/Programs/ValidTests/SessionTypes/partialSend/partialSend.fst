f : Bool -> !Int;End -> !Int;?Bool;End 1-> ()
f cond c d =
  let x = send @Int 5 in  -- x : ∀b: 1S . !Int;b 1-> b
    if cond
    then x @End c |> close; consumeD d
    else receiveAndClose @Bool (x  @(?Bool;End) d); consumeC c

consumeC : !Int;End -> ()
consumeC c = send 7 c |> close

consumeD : !Int;?Bool;End -> ()
consumeD d = receiveAndClose @Bool (send 7 d); ()

main : Int
main = 5
