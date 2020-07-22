-- Line comment --

{-
  Block comment
  {-
    Nested comment
  -}

  {-
    Another nested comment
  -}
-}

function : (rec x:Sl. &{One: ?Int, Two: !Int, Three: Skip}) -> Int
function c =
  let c = select c Two in
  let c = send c 1 in
  0

data List = Nil | Cons Int List | Yes What

-- Check if the list is empty
caseFunction : List -> Bool
caseFunction l =
  case l of {
    Nil ->
      let b = True in
      b,
    Cons x ll ->
      False
  }
