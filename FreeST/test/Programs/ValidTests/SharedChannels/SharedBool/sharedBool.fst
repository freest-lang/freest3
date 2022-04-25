type Bool' = *+{False', True'}
type Diverge = Char

true' : Bool' -> Diverge
true' c = true' $ select True' c

false' : Bool' -> Diverge
false' c = false' $ select False' c

cond : forall a . dualof Bool' -> a -> a -> a
cond c v1 v2 = match c with {True' _ -> v1, False' _ -> v2}

main : Int
main =
  let (tw, tr) = new Bool' in
  let (fw, fr) = new Bool' in
  fork $ true' tw;
  fork $ false' fw;
  cond [Int] tr 1 2 + cond [Int] fr 3 4
  -- 1
