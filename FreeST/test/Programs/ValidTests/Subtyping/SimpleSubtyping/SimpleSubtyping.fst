writer : +{C: Skip} -> Skip
writer c = select C c

reader : &{C: Skip, D: Skip} -> Skip
reader (C c) = c 
reader (D c) = c 
  
main : Skip
main =
  let (w, r) = new +{C: Skip} in
  let _ = fork @Skip (\_:() 1-> writer w) in
  reader r
