-- {T,e} as T

-- let {X,x} = e in e

counterADT : ∃a . (a, a -> Int, a -> a)
counterADT =
  { Int
  , (1, λi:Int -> i, λi:Int -> succ i)
  }
  as
  ∃a . (a, a -> Int, a -> a)

main : Int
main =
  let {counterType, counter} = counterADT in
  let (newc, ops) = counter in
  let (get, inc) = ops in
  get (inc newc)

