import List

stackAdt : ∃a . (a, Int -> a -> a, a -> (Int, a), a -> [Int])
stackAdt =
  { [Int]
  , ( []                              -- new
    , λx:Int xs:[Int] -> x::xs        -- push
    , λxs:[Int] -> (head xs, tail xs) -- top
    , λxs:[Int] -> xs                 -- toList
    )
  }
  as
  ∃a . (a, Int -> a -> a, a -> (Int, a), a -> [Int])

mainStack : Int
mainStack =
  let {stackType, ops} = stackAdt in
  let (news, ops) = ops in
  let (push, ops) = ops in
  let (top, toList) = ops in
  fst @Int @StackType $ top (push 5 (push 7 news))

-- Reversing a list in O(n)

reverse' : [Int] -> [Int]
reverse' = 
  let {stackType, ops} = stackAdt in
  let (news, ops) = ops in
  let (push, ops) = ops in
  let (pop, toList) = ops in
  rev @stackType push toList news

rev : ∀a . (Int -> a -> a) -> (a -> [Int]) -> a -> [Int] -> [Int]
rev push toList stack [] = toList stack
rev push toList stack (x::xs) = rev @a push toList (push x stack) xs

main : [Int]
main = reverse' [1, 2, 3]
