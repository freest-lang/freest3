data IList = Nil | Cons Int IList

type IListW : SL = +{Nil: Skip, Cons: !Int; IListW}

iListW : forall a:SL . IList -> IListW;a -> a

iListW xs c =
  case xs of {
    Nil -> select Nil c,
    Cons x xs -> select Cons c & send x & iListW [a] xs
  }

iListR : forall a:SL . (dualof IListW);a -> (IList, a)
iListR c =
  match c with {
    Nil c -> (Nil, c),
    Cons c -> let (x, c) = receive c in
              let (xs, c) = iListR [a] c in
              (Cons x xs, c)
  }

iListR' : forall a:SL . (dualof IListW);a -> (IList, a)
iListR' c = iFold [IList, a] Nil Cons c

iLength : forall a:SL . (dualof IListW);a -> (Int, a)
iLength c =
  match c with {
    Nil c -> (0, c),
    Cons c -> let (m, c) = receive c in
              let (n, c) = iLength [a] c in
              (m + n, c)
  }

iLength' : forall a:SL . (dualof IListW);a -> (Int, a)
iLength' x = iFold [Int, a] 0 (+) x

iFold : forall a:TL b:SL .
  a -> (Int -> a -> a) -o (dualof IListW);b -o (a, b)
iFold n f c =
  match c with {
    Nil c -> (n, c),
    Cons c -> let (m, c) = receive c in
              let (n, c) = iFold [a, b] n f c in
              (f m n, c)
  }

aList : IList
aList = Cons 5 (Cons 3 (Cons 7 (Cons 1 Nil)))

main : Int
main = let (w, r) = new IListW in
       fork[Skip] $ iListW [Skip] aList w;
       fst [Int, Skip] $ iLength' [Skip] r

-- main : IList
-- main = let (w, r) = new IListW in
--        fork (sink [Skip] $ iListW [Skip] aList w);
--        fst [IList, Skip] $ iListR' [Skip] r
