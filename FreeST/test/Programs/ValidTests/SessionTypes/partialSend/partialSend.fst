f : Bool -> !Int;End -> !Int;?Bool;End 1-> ()
f cond c d =
  let x = send @Int 5 in  -- x : ∀b: 1S . !Int;b 1-> b
    if cond
    then x @End c & close; consumeD d
    else let (_, d) = receive (x  @(?Bool;End) d) in close d; consumeC c

consumeC : !Int;End -> ()
consumeC c = send 7 c & close

consumeD : !Int;?Bool;End -> ()
consumeD d = let (_, d) = receive (send 7 d) in close d

main : Int
main = 5
