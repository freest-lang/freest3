main : Bool
main =
  let (w, r) = new &{B: !Bool} in
  let _ = fork (f w) in
  let (x, _) = f1 r in
  x

type F = +{B: !Bool}

<

f : F -> Skip
f c = let c = select c B in send c True

f1 : dualof F -> (Bool, Skip)
f1 c = match c with { B c -> receive c }


