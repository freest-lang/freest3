

f : !Char;a -> a
f c = let c = send 'a' c in c

g : ?Char;a -> a
g c = let (_,c) = receive c in c


main : ()
main =
  let (w,r) = new @(!Char;T) () in
  fork (\_:() 1-> f @T w |> writer 0) ;
  g @(dualof T) r |> reader

type T = !Int;T;?Int

writer : Int -> T -> ()
writer i c =
  let _ = writer (i + 1) (send i c)
  in ()

reader : dualof T -> ()
reader c =
  let (i, c) = receive c in
  print @Int i;
  reader c
