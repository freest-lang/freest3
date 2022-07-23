type Chan : 1S = +{Done: End, More: !Int;Chan}

fives : Int -> Chan -> ()
fives n c =
  if n == 0
  then select Done c & close
  else fives (n-1) (select More c & send 5)

sumFives : dualof Chan -> Int
sumFives c =
  match c with {
    Done c -> close c; 0,
    More c ->
     let (n, c) = receive c in
     n + sumFives c
  }

main : Int
main =
  let (w, r) = new Chan in
  let _ = fork @() (fives 32 w) in
  sumFives r
