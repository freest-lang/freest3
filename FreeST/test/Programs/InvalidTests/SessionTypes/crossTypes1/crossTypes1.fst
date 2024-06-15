type ListOut = +{Nil : Skip, Cons: !Int;ListOut}
type ListIn = dualof ListOut

rcvList : ListOut;a -> ([Int], a)
rcvList c =
  match c with {
    Cons c ->
      let (i, c) = receive c in
      let (xs, c) = rcvList @a c in
      (i::xs, c),
    Nil c -> ([], c)
  }

sendList : [Int] -> ListIn;a -> a
sendList []     c = select Nil c
sendList (x::xs) c = c |> select Cons |> send x |> sendList @a xs


main : [Int]
main =
  let (x, y) = new @(ListOut;Close) () in
  let _      = fork @() (\_:() 1-> sendList @Close x aList |> close) in
  let (list, y) = rcvList @Wait y in
  wait y; 
  list

aList : [Int]
aList = [2,3,4,5]
