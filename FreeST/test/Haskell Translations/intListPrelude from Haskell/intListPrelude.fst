data List = Nil | Cons Int List

--map :: (a -> b) -> [a] -> [b]
map' : (Int -> Int) -> List -> List
map' f l =
    case l of {
        Nil -> Nil,
        Cons x rest -> Cons (f x) (map' f rest)
    }

--(++) :: [a] -> [a] -> [a]
plusplus : List -> List -> List
plusplus l1 l2 =
    case l1 of {
        Nil -> l2,
        Cons x rest -> Cons x (plusplus rest l2)
    }

--filter :: (a -> Bool) -> [a] -> [a]
filter' : (Int -> Bool) -> List -> List
filter' f l =
    case l of {
        Nil -> Nil,
        Cons x rest ->
            if f x
                then filter' f rest
                else Cons x (filter' f rest)
    }

data Maybe = Nothing | Just Int

--head :: [a] -> a
safeHead' : List -> Maybe
safeHead' l = 
    case l of {
        Nil -> Nothing,
        Cons x _ -> Just x
    }

--last :: [a] -> a
safeLast' : List -> Maybe
safeLast' l = 
    case l of {
        Nil -> Nothing,
        Cons x rest -> 
            case rest of {
                Nil -> Just x,
                Cons _ _ -> safeLast' rest 
            }
    }

--tail :: [a] -> [a]
tail' : List -> List
tail' l =
    case l of {
        Nil -> Nil,
        Cons _ rest -> rest
    }

--init :: [a] -> [a]
init' : List -> List
init' l =
    case l of {
        Nil -> Nil,
        Cons x rest -> 
            case rest of {
                Nil -> Nil,
                Cons _ _ -> Cons x (init' rest) 
            }
    }

--null :: Foldable t => t a -> Bool
null' : List -> Bool
null' l =
    case l of {
        Nil -> True,
        Cons _ _ -> False
    }

--length :: Foldable t => t a -> Int
length' : List -> Int
length' l = 
    case l of {
        Nil -> 0,
        Cons _ rest -> 1 + (length' rest)
    }

--(!!) :: [a] -> Int -> a
safeGetIndex' : List -> Int -> Maybe
safeGetIndex' l i = 
    case l of {
        Nil -> Nothing,
        Cons x rest ->
            if i == 0
                then Just x
                else safeGetIndex' rest (i-1)
    }

--reverse :: [a] -> [a]
reverse' : List -> List
reverse' l = reverse'' l Nil

reverse'' : List -> List -> List
reverse'' from to =
    case from of {
        Nil -> to,
        Cons x rest -> reverse'' rest (Cons x to)
    }

-- "and", "or" and "any" modified to work for Lists, now f (Int->Bool) is a parameter

--and :: Foldable t => t Bool -> Bool
andf' : (Int -> Bool) -> List ->  Bool
andf' f l =
    case l of {
        Nil -> True,
        Cons x rest ->
            if f x 
                then andf' f rest
                else False
    } 

--or :: Foldable t => t Bool -> Bool
orf' : (Int -> Bool) -> List ->  Bool
orf' f l =
    case l of {
        Nil -> False,
        Cons x rest -> 
            if f x
                then True
                else orf' f rest
    }

--any :: Foldable t => (a -> Bool) -> t a -> Bool
any' : (Int -> Bool) -> List -> Bool
any' f l = orf' f l

--all :: Foldable t => (a -> Bool) -> t a -> Bool
all' : (Int -> Bool) -> List -> Bool
all' f l = andf' f l

data IntLCons = ENil | LCons List IntLCons

--concat :: Foldable t => t [a] -> [a]
concat' : IntLCons -> List
concat' ll =
    case ll of {
        ENil -> Nil,
        LCons l rest -> plusplus l (concat' rest) 
    }

--concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap' : (Int -> List) -> List -> List
concatMap' f l =
    case l of {
        Nil -> Nil,
        Cons x rest -> plusplus (f x) (concatMap' f rest)
    }


-- Note: last (scanl f z xs) == foldl f z xs.

--scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl' : (Int -> Int -> Int) -> Int -> List -> List
scanl' f acc l = Cons acc (scan' f acc l)

scan' : (Int -> Int -> Int) -> Int -> List -> List
scan' f acc l = 
    case l of {
        Nil -> Nil,
        Cons x rest -> Cons (f x acc) (scan' f (f x acc) rest)
    }

--scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1' : (Int -> Int -> Int) -> List -> List
scanl1' f l =
    case l of {
        Nil -> Nil,
        Cons x rest -> Cons x (scan' f x rest)
    }

--scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr' : (Int -> Int -> Int) -> Int -> List -> List
scanr' f acc l = reverse' (scanl' f acc (reverse' l))

--scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1' : (Int -> Int -> Int) -> List -> List
scanr1' f l = reverse' (scanl1' f (reverse' l))


-- INFINITE LOOPS --

--iterate :: (a -> a) -> a -> [a]
iterate' : (Int -> Int) -> Int -> List
iterate' f x = Cons (f x) (iterate' f (f x))

--repeat :: a -> [a]
repeat' : Int -> List
repeat' x = iterate' (\x : Int -> x) x

--replicate :: Int -> a -> [a]
replicate' : Int -> Int -> List
replicate' n x = if n == 0 then Nil else Cons x (replicate' (n-1) x)

--cycle :: [a] -> [a]
cycle' : List -> List
cycle' l = cycle' (plusplus l l)

--------------------

--take :: Int -> [a] -> [a]
take' : Int -> List -> List
take' i l =
    case l of {
        Nil -> Nil,
        Cons x rest -> 
            if i == 0
                then Nil
                else Cons x (take' (i-1) rest)
    }

--drop :: Int -> [a] -> [a]
drop' : Int -> List -> List
drop' i l =
    case l of {
        Nil -> Nil,
        Cons x rest -> 
            if i == 0
                then l
                else drop' (i-1) rest
    }

data ILTuple = ILT List List

--splitAt :: Int -> [a] -> ([a], [a])
splitAt' : Int -> List -> ILTuple
splitAt' n l = ILT (take' n l) (drop' n l)

--takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile' : (Int -> Bool) -> List -> List
takeWhile' f l =
    case l of {
        Nil -> Nil,
        Cons x rest -> 
            if f x
                then Cons x (takeWhile' f rest)
                else Nil
    }

--dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile' : (Int -> Bool) -> List -> List
dropWhile' f l =
    case l of {
        Nil -> Nil,
        Cons x rest -> 
            if f x
                then dropWhile' f rest
                else l
    }

--span :: (a -> Bool) -> [a] -> ([a], [a])
span' : (Int -> Bool) -> List -> ILTuple
span' f l = ILT (takeWhile' f l) (dropWhile' f l)

--break :: (a -> Bool) -> [a] -> ([a], [a])
break' : (Int -> Bool) -> List -> ILTuple
break' f l = span' (\x : Int -> not (f x)) l

--notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem' : Int -> List -> Bool
notElem' x l = 
    case l of {
        Nil -> True,
        Cons y rest -> 
            if x == y
                then False
                else notElem' x rest
    }

data ITuple = IT Int Int
data ITCons = ITNil | ITL ITuple ITCons

--lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup' : Int -> ITCons -> Maybe
lookup' x tl =
    case tl of {
        ITNil -> Nothing,
        ITL tuple rest -> 
            case tuple of {
                IT a b -> 
                    if x == a
                        then Just b
                        else lookup' x rest
            }
    }

--zip :: [a] -> [b] -> [(a, b)]
zip' : List -> List -> ITCons
zip' l1 l2 =
    case l1 of {
        Nil -> ITNil,
        Cons a rest1 ->
            case l2 of {
                Nil -> ITNil,
                Cons b rest2 -> ITL (IT a b) (zip' rest1 rest2)
            }
    }

data ITuple3 = IT3 Int Int Int
data IT3Cons = IT3Nil | IT3L ITuple3 IT3Cons

--zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' : List -> List -> List -> IT3Cons
zip3' l1 l2 l3 =
    case l1 of {
        Nil -> IT3Nil,
        Cons a rest1 ->
            case l2 of {
                Nil -> IT3Nil,
                Cons b rest2 -> 
                    case l3 of {
                        Nil -> IT3Nil,
                        Cons c rest3 -> IT3L (IT3 a b c) (zip3' rest1 rest2 rest3)
                    }
            }
    }


--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' : (Int -> Int -> Int) -> List -> List -> List
zipWith' f l1 l2 =
    case l1 of {
        Nil -> Nil,
        Cons a rest1 ->
            case l2 of {
                Nil -> Nil,
                Cons b rest2 -> Cons (f a b) (zipWith' f rest1 rest2) 
            }
    }

--zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' : (Int -> Int -> Int -> Int) -> List -> List -> List -> List
zipWith3' f l1 l2 l3=
    case l1 of {
        Nil -> Nil,
        Cons a rest1 ->
            case l2 of {
                Nil -> Nil,
                Cons b rest2 -> 
                    case l3 of {
                        Nil -> Nil,
                        Cons c rest3 -> Cons (f a b c) (zipWith3' f rest1 rest2 rest3) 
                    }
            }
    }

--unzip :: [(a, b)] -> ([a], [b])
unzip' : ITCons -> ILTuple
unzip' l = ILT (unzipA l) (unzipB l)

unzipA : ITCons -> List
unzipA tl =
    case tl of {
        ITNil -> Nil,
        ITL tuple rest ->
            case tuple of {
                IT a _ -> Cons a (unzipA rest)
            }
    }

unzipB : ITCons -> List
unzipB tl =
    case tl of {
        ITNil -> Nil,
        ITL tuple rest ->
            case tuple of {
                IT _ b -> Cons b (unzipB rest)
            }
    }

data IL3Tuple = IL3T List List List

--unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3' : IT3Cons -> IL3Tuple
unzip3' l = IL3T (unzipA' l) (unzipB' l) (unzipC' l)

unzipA' : IT3Cons -> List
unzipA' tl =
    case tl of {
        IT3Nil -> Nil,
        IT3L tuple rest ->
            case tuple of {
                IT3 a _ _ -> Cons a (unzipA' rest)
            }
    }

unzipB' : IT3Cons -> List
unzipB' tl =
    case tl of {
        IT3Nil -> Nil,
        IT3L tuple rest ->
            case tuple of {
                IT3 _ b _ -> Cons b (unzipB' rest)
            }
    }

unzipC' : IT3Cons -> List
unzipC' tl =
    case tl of {
        IT3Nil -> Nil,
        IT3L tuple rest ->
            case tuple of {
                IT3 _ _ c -> Cons c (unzipC' rest)
            }
    }

-- Strings

data Str = SNil | Str Char Str
data StrCons = SLNil | StrL Str StrCons

--lines :: String -> [String]
lines' : Str -> StrCons
lines' str = lines'' (takeUntilf str isBreak) (dropUntilf str isBreak)

lines'' : Str -> Str -> StrCons 
lines'' str1 str2 = 
    case str1 of {
        SNil -> SLNil,
        Str _ _ -> StrL str1 (lines' str2)
    }

isBreak : Char -> Bool
isBreak c = ((ord c) == (ord '\n'))

takeUntilf : Str -> (Char -> Bool) -> Str
takeUntilf str f =
    case str of {
        SNil -> SNil,
        Str c rest -> 
            if f c
                then SNil
                else Str c (takeUntilf rest f)
    } 

dropUntilf : Str -> (Char -> Bool) -> Str
dropUntilf str f =
    case str of {
        SNil -> SNil,
        Str c rest -> 
            if f c
                then rest
                else dropUntilf rest f
    } 

--words :: String -> [String]
words' : Str -> StrCons
words' str = words'' (takeUntilf str isSpace) (dropUntilf str isSpace)

words'' : Str -> Str -> StrCons 
words'' str1 str2 = 
    case str1 of {
        SNil -> SLNil,
        Str _ _ -> StrL str1 (words' str2)
    }

isSpace : Char -> Bool
isSpace c = ((ord c) == (ord '\n')) || ((ord c) == (ord ' '))

--unlines :: [String] -> String
unlines' : StrCons -> Str
unlines' sl =
    case sl of {
        SLNil -> SNil,
        StrL str rest -> 
            case rest of {
                SLNil -> str,
                StrL _ _ -> concatStr (concatStr str (Str '\n' SNil)) (unlines' rest)
            }
    }

concatStr : Str -> Str -> Str
concatStr s1 s2 =
    case s1 of {
        SNil -> s2,
        Str c rest -> Str c (concatStr rest s2)
    }

--unwords :: [String] -> String
unwords' : StrCons -> Str
unwords' sl =
    case sl of {
        SLNil -> SNil,
        StrL str rest -> 
            case rest of {
                SLNil -> str,
                StrL _ _ -> concatStr (concatStr str (Str ' ' SNil)) (unwords' rest)
            }
    }

list1 : List
list1 = makeNCons 10

list2 : List
list2 = makeNCons 2

main : Bool
main = 
    let a1 = map' (\x : Int -> x+1) (makeNCons 2) in
    let a = equalL a1 (Cons 3 (Cons 2 Nil)) in

    let b1 = plusplus a1 a1 in
    let b = equalL b1 (Cons 3 (Cons 2 (Cons 3 (Cons 2 Nil)))) in

    let c1 = (Cons 5 (Cons 5 (Cons 2 (Cons 1 (Cons 5 Nil))))) in
    let c = equalL (filter' (\x:Int -> x==5) c1) (Cons 2 (Cons 1 Nil)) in

    let d = equalMaybe (safeHead' list1) 10 in

    let e = equalMaybe (safeLast' list1) 1 in

    let f = equalL (tail' list1) (makeNCons 9) in

    let g1 = (Cons 1 (Cons 2 (Cons 3 Nil))) in
    let g = equalL (init' g1) (Cons 1 (Cons 2 Nil)) in

    let h = null' Nil && not (null' list1) in

    let i = (length' list1 == 10) && (length' (Cons 1 Nil) == 1) in
    
    let j1 = safeGetIndex' list1 5 in
    let j2 = safeGetIndex' list1 11 in
    let j = equalMaybe j1 5 && isNothing j2 in

    let k1 = reverse' list1 in
    let k = equalL k1 
            (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Nil)))))))))) in

    let l1 = (\x : Int -> x > 0) in
    let l2 = (\x : Int -> x < 5) in
    let l = (andf' l1 list1) && not (andf' l2 list1) in

    let m1 = (\x : Int -> x == 5) in
    let m2 = (\x : Int -> x <= 0) in
    let m = (orf' m1 list1) && not (orf' m2 list1) in

    let n = (any' m1 list1) && not (any' m2 list1) in

    let o1 = (\x : Int -> x /= 0) in
    let o = (all' o1 list1) && not (all' m1 list1) in

    let p1 = LCons list2 (LCons list2 (LCons list2 ENil)) in
    let p2 = Cons 2 (Cons 1 (Cons 2 (Cons 1 (Cons 2 (Cons 1 Nil))))) in
    let p = equalL (concat' p1) p2 in 

    let q1 = (\x : Int -> Cons x (Cons x Nil)) in
    let q2 = Cons 2 (Cons 2 (Cons 1 (Cons 1 Nil))) in
    let q = equalL (concatMap' q1 list2) q2 in

    let r1 = (\x : Int -> (\y : Int -> x + y)) in
    let r2 = Cons 0 (Cons 2 (Cons 3 Nil)) in
    let r = equalL (scanl' r1 0 list2) r2 in

    let s = equalL (scanl1' r1 list2) (Cons 2 (Cons 3 Nil)) in

    let t = equalL (scanr' r1 0 list2) (Cons 3 (Cons 1 (Cons 0 Nil))) in

    let u = equalL (scanr1' r1 list2) (Cons 3 (Cons 1 Nil)) in

    --iterate
    --repeat

    let v = equalL (replicate' 2 10) (Cons 10 (Cons 10 Nil)) in

    --cycle

    let w = equalL (take' 3 list1)  (Cons 10 (Cons 9 (Cons 8 Nil))) in

    let x = equalL (drop' 8 list1) (Cons 2 (Cons 1 Nil)) in

    let y1 = splitAt' 5 list1 in
    let y2 = equalL (fst' y1) (Cons 10 (Cons 9 (Cons 8 (Cons 7 (Cons 6 Nil))))) in
    let y3 = equalL (snd' y1) (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))) in
    let y = y2 && y3 in

    let z1 = (\x : Int -> x > 5) in
    let z = equalL (takeWhile' z1 list1) (Cons 10 (Cons 9 (Cons 8 (Cons 7 (Cons 6 Nil))))) in

    let aa = equalL (dropWhile' z1 list1) (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))) in

    let ab1 = span' z1 list1 in
    let ab2 = equalL (fst' ab1) (Cons 10 (Cons 9 (Cons 8 (Cons 7 (Cons 6 Nil))))) in
    let ab3 = equalL (snd' ab1) (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))) in
    let ab = ab2 && ab3 in

    let ac1 = break' z1 list1 in
    let ac2 = equalL (fst' ac1) Nil in
    let ac3 = equalL (snd' ac1) list1 in
    let ac = ac2 && ac3 in

    let ad = (notElem' 0 list1) && not (notElem' 1 list1) in

    let ae1 = ITL (IT 10 1) (ITL (IT 5 5) (ITL (IT 1 10) ITNil)) in
    let ae = (equalMaybe (lookup' 1 ae1) 10) && (isNothing (lookup' 20 ae1)) in

    let af = equalTL (zip' list1 list2) (ITL (IT 10 2) (ITL (IT 9 1) ITNil)) in

    let ag = equalT3L (zip3' list1 list2 list2) (IT3L (IT3 10 2 2) (IT3L (IT3 9 1 1) IT3Nil)) in

    let ah1 = (\x : Int -> (\y : Int -> x - y)) in
    let ah = equalL (zipWith' ah1 list1 list2) (Cons 8 (Cons 8 Nil)) in

    let aj1 = (\x : Int -> (\y : Int -> (\z : Int -> x-(y-z)))) in
    let aj = equalL (zipWith3' aj1 list1 list1 list2) (Cons 2 (Cons 1 Nil)) in

    let ak1 = zip' list1 list2 in
    let ak2 = unzip' ak1 in
    let ak = equalL (fst' ak2) (Cons 10 (Cons 9 Nil)) && equalL (snd' ak2) (Cons 2 (Cons 1 Nil)) in

    let al1 = zip3' list1 list2 (makeNCons 5) in
    let al2 = unzip3' al1 in
    let al3 = (Cons 10 (Cons 9 Nil)) in
    let al4 = (Cons 2 (Cons 1 Nil)) in
    let al5 = (Cons 5 (Cons 4 Nil)) in
    let al = equalL (fst'' al2) al3 && equalL (snd'' al2) al4 && equalL (trd'' al2) al5 in

    let am1 = Str 'o' (Str 'i' (Str '\n' (Str '.' SNil))) in
    let am2 = StrL (Str 'o' (Str 'i' SNil)) (StrL (Str '.' SNil) SLNil) in
    let am = equalStrL (lines' am1) am2 in

    let an1 = Str 'o' (Str 'i' (Str ' ' am1)) in
    let an2 = StrL (Str 'o' (Str 'i' SNil)) am2 in
    let an = equalStrL (words' an1) an2 in

    let ao = equalStr am1 (unlines' am2) in

    let ap1 = Str 'o' (Str 'i' (Str ' ' (Str 'o' (Str 'i' (Str ' ' (Str '.' SNil)))))) in
    let ap = equalStr ap1 (unwords' an2) in

    let result = a && b && c && d && e && f && g && h && j && k && l && m 
                && n && o && p && q && r && s && t && u && v && w && x && y
                && z && aa && ab && ac && ad && ae && af && ag && ah && aj 
                && ak && al && am && an && ao && ap in
    result

-- Auxiliary methods for main testing

equalMaybe : Maybe -> Int -> Bool
equalMaybe x b =
    case x of {
        Nothing -> False,
        Just a -> a == b
    }

isNothing : Maybe -> Bool
isNothing n = case n of { Nothing -> True, Just _ -> False }

equalL : List -> List -> Bool
equalL l1 l2 =
    case l1 of {
        Nil -> 
            case l2 of {
                Nil -> True,
                Cons _ _ -> False
            },
        Cons x rest1 ->
            case l2  of {
                Nil -> False,
                Cons y rest2 ->
                    if x == y
                        then equalL rest1 rest2
                        else False
            }
    }

equalT : ITuple -> ITuple -> Bool
equalT t1 t2 =
    case t1 of {
        IT a b ->
            case t2 of {
                IT c d -> (a == c) && (b == d)
            }
    }

equalTL : ITCons -> ITCons -> Bool
equalTL l1 l2 =
    case l1 of {
        ITNil -> 
            case l2 of {
                ITNil -> True,
                ITL _ _ -> False
            },
        ITL tuple1 rest1 -> 
            case l2 of {
                ITNil -> False,
                ITL tuple2 rest2 -> 
                    if equalT tuple1 tuple2
                        then equalTL rest1 rest2
                        else False 
            }
    }

equalT3 : ITuple3 -> ITuple3 -> Bool
equalT3 t1 t2 =
    case t1 of {
        IT3 a b c ->
            case t2 of {
                IT3 d e g -> (a == d) && (b == e) && (c == g) 
            }
    }

equalT3L : IT3Cons -> IT3Cons -> Bool 
equalT3L l1 l2 =
    case l1 of {
        IT3Nil -> 
            case l2 of {
                IT3Nil -> True,
                IT3L _ _ -> False
            },
        IT3L tuple1 rest1 -> 
            case l2 of {
                IT3Nil -> False,
                IT3L tuple2 rest2 -> 
                    if equalT3 tuple1 tuple2
                        then equalT3L rest1 rest2
                        else False 
            }
    }

makeNCons : Int -> List
makeNCons n = if n == 0 then Nil else Cons n (makeNCons (n-1))

fst' : ILTuple -> List
fst' tuple = case tuple of { ILT first _ -> first}

snd' : ILTuple -> List
snd' tuple = case tuple of { ILT _ second -> second}

fst'' : IL3Tuple -> List
fst'' tuple = case tuple of { IL3T first _ _-> first}

snd'' : IL3Tuple -> List
snd'' tuple = case tuple of { IL3T _ second _ -> second}

trd'' : IL3Tuple -> List
trd'' tuple = case tuple of { IL3T _ _ third -> third}

equalStr : Str -> Str -> Bool
equalStr s1 s2 =
    case s1 of {
        SNil ->
            case s2 of {
                SNil -> True,
                Str _ _ -> False
            },
        Str c1 rest1 ->
            case s2 of {
                SNil -> False,
                Str c2 rest2 -> 
                    if (ord c1) == (ord c2)
                        then equalStr rest1 rest2 
                        else False
            }
    }

equalStrL : StrCons -> StrCons -> Bool
equalStrL sl1 sl2 =
    case sl1 of {
        SLNil ->
            case sl2 of {
                SLNil -> True,
                StrL _ _ -> False
            },
        StrL s1 rest1 ->
            case sl2 of {
                SLNil -> False,
                StrL s2 rest2 ->
                    if equalStr s1 s2
                        then equalStrL rest1 rest2
                        else False
            }
    }
