data List a = Cons a (List a) | Nil

main1 : List Int
main1 = Cons[Int] 1 $ Cons [Int] 2 $ Cons [Int] 3 $ Cons [Int] 4 $ Nil[Int]

main2 : List Char
main2 = Cons[Char] 'a' $ Cons [Char] 'b' $ Cons [Char] 'c' $ Nil[Char]

zip : ∀ a b . List a → List b → List (a,b)
zip xs ys =
  case xs of {
    Cons x xs ->
      case ys of {
        Cons y ys -> Cons[(a,b)] (x,y) (zip[a,b] xs ys),
        Nil -> Nil[(a,b)]
      },
    Nil -> Nil[(a,b)]
  }


main : List (Int, Char)
main = zip[Int, Char] main1 main2