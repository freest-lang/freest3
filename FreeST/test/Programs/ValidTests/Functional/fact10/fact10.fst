type Choice : 1S = +{More: !Int;Choice, Enough: Skip}

sendInt : ∀ a:1S . Int -> Choice;a -> a
sendInt i c =
  if i == 0 then
    select Enough c
  else
    let c = select More c in
    let c = send i c in
    sendInt @a (i - 1) c

rcvInt : ∀ a:1S . Int -> (dualof Choice);a -> (Int, a)
rcvInt acc c =
  match c with {
    Enough c -> (acc,c),
    More c ->
      let (i, c) = receive c in
      let (iii, c) = rcvInt @a (acc*i) c in
      (iii, c)
  }

main : Int
main =
  let (w, r) = new Choice;End in
  let _ = fork @() (sendInt @End 10 w & close) in
  let (i, r) = rcvInt @End 1 r in
  close r; 
  i
