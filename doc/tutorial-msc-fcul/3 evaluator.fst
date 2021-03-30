data Exp = Lit Int | Plus Exp Exp | Times Exp Exp

-- 3 + 2 * 5
e1 : Exp
e1 = Plus (Lit 3) (Times (Lit 2) (Lit 5))

type EChan : SL =
  +{ Lit: !Int
   , Plus: EChan ; EChan
   , Times: EChan ; EChan
   }

writeExp : forall a:SL . Exp -> EChan;a -> a
writeExp e c = case e of
  { Lit n -> send n $ select Lit c
  , Plus e1 e2 ->
      let c1 = select Plus c in
      let c2 = writeExp[EChan;a] e1 c1 in
      writeExp[a] e2 c2
  , Times e1 e2 ->
      let c1 = select Times c in
      let c2 = writeExp[EChan;a] e1 c1 in
      writeExp[a] e2 c2
  }

wExp : Exp -> EChan;?Int -> ()
wExp e c =
  let c1 = writeExp[?Int] e c in
  let (n, _) = receive c1 in
  printIntLn n

readExp : forall a:SL . dualof EChan;a -> (Int, a)
readExp c = match c with
  { Lit c -> receive c
  , Plus c ->
    let (n1, c1) = readExp[dualof EChan;a] c in
    let (n2, c2) = readExp[a] c1 in
    (n1 + n2, c2)
  , Times c ->
    let (n1, c1) = readExp[dualof EChan;a] c in
    let (n2, c2) = readExp[a] c1 in
    (n1 * n2, c2)
  }

rExp : dualof EChan;!Int -> Skip
rExp c =
  let (n, c) = readExp[!Int] c in
  send n c

main : Skip
main =
  let (w, r) = new EChan;?Int in
  fork[()] (wExp e1 w);
  rExp r

{-
main : (Int, Skip)
main =
  let (w, r) = new EChan in
  fork (sink (writeExp[Skip] e1 w));
  readExp[Skip] r
-}

sink : Skip -> ()
sink _ = ()
