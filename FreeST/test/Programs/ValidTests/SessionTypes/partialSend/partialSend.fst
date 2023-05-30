f : Bool -> !Int;EndW -> !Int;?Bool;EndC 1-> ()
f cond c d =
  let x = send @Int 5 in  -- x : ∀b: 1S . !Int;b 1-> b
    if cond
    then x @EndW c |> wait; consumeD d
    else receiveAndClose @Bool (x @(?Bool;EndC) d); consumeC c

consumeC : !Int;EndW -> ()
consumeC c = send 7 c |> wait

consumeD : !Int;?Bool;EndC -> ()
consumeD d = receiveAndClose @Bool (send 7 d); ()

main : Int
main = 5
