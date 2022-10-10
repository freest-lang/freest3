writer : +{A: Skip, B: Skip} -> Skip
writer c = select A c

reader : &{A: Skip, B: Skip, C:Skip} -> ()
reader (A c) = ()
reader (B c) = ()
  
main : ()
main =
  let (w, r) = new +{A: Skip, B: Skip} in
  let _ = fork @Skip (\_:() 1-> writer w) in
  reader r
