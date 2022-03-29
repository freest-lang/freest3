writer : +{C: Skip, D: Skip} -> Skip
writer c = select c D

reader : &{C: Skip} -> Skip
reader c = match c with {
    C c -> c
  }
  
main : Skip
main =
  let (w, r) = new +{C: Skip, D: Skip} in
  let _ = fork[Skip] (writer w) in
  reader r
