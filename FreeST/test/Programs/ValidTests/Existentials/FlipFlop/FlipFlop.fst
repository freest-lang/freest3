{-
Benjamin C. Pierce:
Types and programming languages. MIT Press 2002
-}

counterADT : ∃a . (a, a -> Int, a -> a)
counterADT =
  { Int
  , ( 1                -- new
    , λi:Int -> i      -- get
    , λi:Int -> succ i -- inc
    )
  }
  as
  ∃a . (a, a -> Int, a -> a)

flipFlopADT : ∃a . (a, a -> Bool, a -> a, a -> a)
flipFlopADT =
  let {counterType, counter} = counterADT in
  let (newc, ops) = counter in
  let (get, inc) = b in
  { counterType
  , ( newc                           -- new
    , λc:counterType -> even (get c) -- read
    , λc:counterType -> inc c        --  toggle
    , λc:counterType -> newc         -- reset
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

