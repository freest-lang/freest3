data IList = Nil | Cons Int IList

type IListW : 1S = +{NilC: Skip, ConsC: !Int; IListW}

iListW : forall a: 1S . IList -> IListW;a -> a
iListW xs c =
  case xs of {
    Nil -> select NilC c,
    Cons x xs -> select ConsC c |> send x |> iListW  @a xs
  }

iListR : forall a: 1S . (dualof IListW);a -> (IList, a)
iListR c =
  match c with {
    NilC c -> (Nil, c),
    ConsC c -> let (x, c) = receive c in
              let (xs, c) = iListR  @a c in
              (Cons x xs, c)
  }

iListR' : forall a: 1S . (dualof IListW);a -> (IList, a)
iListR' c = iFold @IList @a Nil Cons c

iLength : forall a: 1S . (dualof IListW);a -> (Int, a)
iLength c =
  match c with {
    NilC c -> (0, c),
    ConsC c -> let (m, c) = receive c in
              let (n, c) = iLength  @a c in
              (m + n, c)
  }

iLength' : forall a: 1S . (dualof IListW);a -> (Int, a)
iLength' x = iFold @Int @a 0 (+) x

iFold : forall a: 1T b: 1S .
  a -> (Int -> a -> a) 1-> (dualof IListW);b 1-> (a, b)
iFold n f c =
  match c with {
    NilC c -> (n, c),
    ConsC c -> let (m, c) = receive c in
              let (n, c) = iFold  @a @b n f c in
              (f m n, c)
  }

aList : IList
aList = Cons 5 (Cons 3 (Cons 7 (Cons 1 Nil)))

main : Int
main = let (w, r) = new @(IListW;EndC) () in
       fork @() (\_:()1-> iListW @EndC aList w |> close);
       let (i, r) = iLength' @EndW r in 
       wait r;
       i

-- main : IList
-- main = let (w, r) = new @IListW () in
--        fork (sink [Skip] $ iListW [Skip] aList w);
--        fst [IList, Skip] $ iListR' [Skip] r
