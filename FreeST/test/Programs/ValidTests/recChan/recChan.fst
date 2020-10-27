type Chan : SL = +{Done: Skip, More: !Int;Chan}

fives : Int -> Chan -> Skip
fives n c =
  if n == 0
  then select c Done
  else fives (n-1) (send (select c More) 5)

sumFives : dualof Chan -> Int
sumFives c =
  match c with {
    Done _ ->
      0,
    More c ->
     let (n, c) = receive c in
     n + sumFives c
  }

main : Int
main =
  let (w, r) = new Chan in
  let _ = fork (sink (fives 32 w)) in
  sumFives r

-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
