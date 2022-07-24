
main : Int
main =
  let (w, r) = new !Bool in
  let _ = fork @Skip (\_:() 1-> f w) in
  let (x, _) = f1 r in
  x

type F = !Int

f : F -> Skip
f c = send c 4

f1 : dualof F -> (Int, Skip)
f1 c = receive c
