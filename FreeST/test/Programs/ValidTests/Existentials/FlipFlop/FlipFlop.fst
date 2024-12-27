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

mainCounter : Int
mainCounter =
  let {counterType, ops} = counterADT in
  let (newc, ops) = ops in
  let (get, inc) = ops in
  get (inc newc)

flipFlopADT : ∃a . (a, a -> Bool, a -> a, a -> a)
flipFlopADT =
  let {counterType, ops} = counterADT in
  let (newc, ops) = ops in
  let (get, inc) = ops in
  { counterType
  , ( newc                           -- new
    , λc:counterType -> even (get c) -- read
    , λc:counterType -> inc c        -- toggle
    , λc:counterType -> newc         -- reset
    )
  }
  as
  ∃a . (a, a -> Bool, a -> a, a -> a)

main : Bool
main =
  let {fFType, flipflop} = flipFlopADT in
  let (newff, ops) = flipflop in
  let (read, ops) = ops in
  let (toggle, reset) = ops in
  read (toggle (reset (toggle newff)))
  
