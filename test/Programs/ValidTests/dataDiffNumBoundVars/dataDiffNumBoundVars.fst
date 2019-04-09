data T = C Int | D

main : Int
main = let _ =
  case C 5 of {
    C x -> 5;
    D -> 7
  } in x
