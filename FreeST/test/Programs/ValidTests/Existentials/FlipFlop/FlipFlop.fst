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

flipFlopADT : ∃a . (a, a -> Bool, a -> a, a -> a)
flipFlopADT =
  let {a, counter} = counterADT in
  let (newc, ops) = counter in
  let (get, inc) = ops in
  { a
  , ( newc
    , λc:a -> even (get c)
    , λc:a -> inc c
    , λc:a -> new
    )
  }
  as
  ∃a . (a, a -> Bool, a -> a, a -> a)

main : Int
main =
  let {counterType, counter} = counterADT in
  let (newc, ops) = counter in
  let (get, inc) = ops in
  get (inc newc)

