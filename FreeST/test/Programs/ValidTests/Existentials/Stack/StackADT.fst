import List

stackAdt : ∃a . (a, Int -> a -> a, a -> a, a -> Int)
stackAdt =
  { [Int]
  , ( []                       -- new
    , λx:Int xs:[Int] -> x::xs -- push
    , λxs:[Int] -> tail xs     -- pop
    , λxs:[Int] -> head xs     -- top
    )
  }
  as
  ∃a . (a, Int -> a -> a, a -> a, a -> Int)

rev : ∀a . (Int -> a -> a) -> (a -> a) ->  (a -> Int) -> a -> [Int] -> [Int]
rev push pop top stack [] = []
rev push pop top stack (x::xs) = rev @a push pop top (push x stack) xs

reverse' : [Int] -> [Int]
reverse' = 
  let {stackType, ops} = stackAdt in
  let (news, ops) = ops in
  let (push, ops) = ops in
  let (pop, top) = ops in
  rev @stackType push pop top news

main : [Int]
main = reverse' [1, 2, 3]

mainStack : Int
mainStack =
  let {stackType, ops} = stackAdt in
  let (news, ops) = ops in
  let (push, ops) = ops in
  let (pop, top) = ops in
  top (push 5 (push 7 news))
