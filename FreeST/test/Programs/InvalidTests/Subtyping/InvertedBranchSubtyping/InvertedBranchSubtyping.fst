writer : +{A: Skip, B: Skip, C: Skip} -> Skip
writer c = select C c

reader : &{A: Skip, B: Skip}-> Skip
reader (A c) = c
reader (B c) = c
  
main : Skip
main =
  let (w, r) = new +{A: Skip, B: Skip, C:Skip} in
  let _ = fork @Skip (\_:() 1-> writer w) in
  reader r
