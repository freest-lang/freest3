type Choice : 1S = +{More: !Int;DD, Enough: End}
type DD : 1S = dualof (dualof Choice)

sendInt : Int -> DD -> ()
sendInt i c =
  c |> select More
    |> send i 
    |> select More
    |> send (i + 1) 
    |> select More 
    |> send (i + 2) 
    |> select Enough
    |> close

rcvInt : Int -> dualof DD -> Int
rcvInt acc c =
  match c with {
    Enough c -> close c; acc,
    More c ->
      let (i, c) = receive c in
      rcvInt (acc+i) c
  }

main : Int
main =
  let (w,r) = new @DD () in
  fork @() (\_:()1-> sendInt 0 w); 
  rcvInt 0 r
