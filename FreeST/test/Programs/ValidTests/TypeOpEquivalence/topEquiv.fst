type Id : (TU -> TU) a : TU = a

f1 : Int → Bool
f2 : Id Int → Bool
f3 : Int → Id Bool
f4 : Id (Int → Bool)
f5 : Id Int → Id Bool
f6 : Id (Id (Int → Bool))
f7 : Id (Id Int → Bool)
f8 : Id (Id (Id (Int → Bool)))
f9 : Id (Id (Id Int → Bool))

f1 _ = False
f2 _ = False
f3 _ = False
f4 _ = False
f5 _ = False
f6 _ = True
f7 _ = True
f8 _ = True
f9 _ = True

main : Id Bool
main =  f1 1 || f2 2 || f3 5
       || f4 1 || f5 2 || f6 5
       || f7 1 || f8 2 || f9 5 