type Chan : 1S = +{Done: Skip, More: !Int;Chan}

fives : Int -> Chan -> Skip
fives n c =
  if n == 0
  then select Done c
  else fives (n-1) (send 5 (select More c))

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
  let _ = fork @Skip \_:() 1-> fives 32 w in
  sumFives r
