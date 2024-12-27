{-
Benjamin C. Pierce:
Types and programming languages. MIT Press 2002
-}
counterADT : ∃a . (a, a -> Int, a -> a)
counterADT =
  { Int
  , (1, λi:Int -> i, λi:Int -> succ i)
  }
  as
  ∃a . (a, a -> Int, a -> a)

main : Int
main =
  let {counterType, ops} = counterADT in
  let (newc, ops) = ops in
  let (get, inc) = ops in
  get (inc newc)

