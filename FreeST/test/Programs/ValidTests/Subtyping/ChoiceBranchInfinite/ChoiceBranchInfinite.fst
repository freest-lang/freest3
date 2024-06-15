type CB1 = +{A: !Int};&{A: ?Int, B: !Bool};CB1
type CB2 = +{A: !Int, B:?Bool};&{A: ?Int };CB2

f : CB1 -> ()
f c = 
  let c = c |> select A |> send 5 in 
  match c with { A c -> let (_, c) = receive c in f c
               , B c -> c |> send True |> f
               }

g : CB2 -> ()
g c = f c

main : () 
main = ()