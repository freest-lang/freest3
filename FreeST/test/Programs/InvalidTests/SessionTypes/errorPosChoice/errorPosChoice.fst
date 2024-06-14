type F : 1S = +{B: !Bool;Close}

f : F -> ()
f c = let c = select B c in send True c |> close

f1 : dualof F -> (Bool, Wait)
f1 c = match c with { B c -> receive c }

main : Bool
main =
  let (w, r) = new @(&{B: !Bool};Close) () in
  fork @() (\_:() 1-> f w);
  let (x, c) = f1 r in
  wait c;
  x



