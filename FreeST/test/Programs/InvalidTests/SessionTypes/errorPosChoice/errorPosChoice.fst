main : Bool
main =
  let (w, r) = new &{B: !Bool} in
  fork @() (\_:()-> f w);
  let (x, c) = f1 r in
  close c;
  x

type F : 1S = +{B: !Bool};End

f : F -> ()
f c = let c = select c B in send c True |> close

f1 : dualof F -> (Bool, End)
f1 c = match c with { B c -> receive c }


