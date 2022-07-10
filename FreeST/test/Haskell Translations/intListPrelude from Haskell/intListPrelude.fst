data IntList = End | List Int IntList

--map :: (a -> b) -> [a] -> [b]
map' : (Int -> Int) -> IntList -> IntList
map' f l =
    case l of {
        End -> End,
        List x rest -> List (f x) (map' f rest)
    }

--(++) :: [a] -> [a] -> [a]
plusplus : IntList -> IntList -> IntList
plusplus l1 l2 =
    case l1 of {
        End -> l2,
        List x rest -> List x (plusplus rest l2)
    }

--filter :: (a -> Bool) -> [a] -> [a]
filter' : (Int -> Bool) -> IntList -> IntList
filter' f l =
    case l of {
        End -> End,
        List x rest ->
            if f x
                then filter' f rest
                else List x (filter' f rest)
    }

data MaybeInt = Empty | Number Int

--head :: [a] -> a
safeHead' : IntList -> MaybeInt
safeHead' l = 
    case l of {
        End -> Empty,
        List x _ -> Number x
    }

--last :: [a] -> a
safeLast' : IntList -> MaybeInt
safeLast' l = 
    case l of {
        End -> Empty,
        List x rest -> 
            case rest of {
                End -> Number x,
                List _ _ -> safeLast' rest 
            }
    }

--tail :: [a] -> [a]
tail' : IntList -> IntList
tail' l =
    case l of {
        End -> End,
        List _ rest -> rest
    }

--init :: [a] -> [a]
init' : IntList -> IntList
init' l =
    case l of {
        End -> End,
        List x rest -> 
            case rest of {
                End -> End,
                List _ _ -> List x (init' rest) 
            }
    }

--null :: Foldable t => t a -> Bool
null' : IntList -> Bool
null' l =
    case l of {
        End -> True,
        List _ _ -> False
    }

--length :: Foldable t => t a -> Int
length' : IntList -> Int
length' l = 
    case l of {
        End -> 0,
        List _ rest -> 1 + (length' rest)
    }

--(!!) :: [a] -> Int -> a
safeGetIndex' : IntList -> Int -> MaybeInt
safeGetIndex' l i = 
    case l of {
        End -> Empty,
        List x rest ->
            if i == 0
                then Number x
                else safeGetIndex' rest (i-1)
    }

--reverse :: [a] -> [a]
reverse' : IntList -> IntList
reverse' l = reverse'' l End

reverse'' : IntList -> IntList -> IntList
reverse'' from to =
    case from of {
        End -> to,
        List x rest -> reverse'' rest (List x to)
    }

-- "and", "or" and "any" modified to work for IntLists, now f (Int->Bool) is a parameter

--and :: Foldable t => t Bool -> Bool
andf' : (Int -> Bool) -> IntList ->  Bool
andf' f l =
    case l of {
        End -> True,
        List x rest ->
            if f x 
                then andf' f rest
                else False
    } 

--or :: Foldable t => t Bool -> Bool
orf' : (Int -> Bool) -> IntList ->  Bool
orf' f l =
    case l of {
        End -> False,
        List x rest -> 
            if f x
                then True
                else orf' f rest
    }

--any :: Foldable t => (a -> Bool) -> t a -> Bool
any' : (Int -> Bool) -> IntList -> Bool
any' f l = orf' f l

--all :: Foldable t => (a -> Bool) -> t a -> Bool
all' : (Int -> Bool) -> IntList -> Bool
all' f l = andf' f l

data IntLList = EEnd | LList IntList IntLList

--concat :: Foldable t => t [a] -> [a]
concat' : IntLList -> IntList
concat' ll =
    case ll of {
        EEnd -> End,
        LList l rest -> plusplus l (concat' rest) 
    }

--concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap' : (Int -> IntList) -> IntList -> IntList
concatMap' f l =
    case l of {
        End -> End,
        List x rest -> plusplus (f x) (concatMap' f rest)
    }


-- Note: last (scanl f z xs) == foldl f z xs.

--scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl' : (Int -> Int -> Int) -> Int -> IntList -> IntList
scanl' f acc l = List acc (scan' f acc l)

scan' : (Int -> Int -> Int) -> Int -> IntList -> IntList
scan' f acc l = 
    case l of {
        End -> End,
        List x rest -> List (f x acc) (scan' f (f x acc) rest)
    }

--scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1' : (Int -> Int -> Int) -> IntList -> IntList
scanl1' f l =
    case l of {
        End -> End,
        List x rest -> List x (scan' f x rest)
    }

--scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr' : (Int -> Int -> Int) -> Int -> IntList -> IntList
scanr' f acc l = reverse' (scanl' f acc (reverse' l))

--scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1' : (Int -> Int -> Int) -> IntList -> IntList
scanr1' f l = reverse' (scanl1' f (reverse' l))


-- INFINITE LOOPS --

--iterate :: (a -> a) -> a -> [a]
iterate' : (Int -> Int) -> Int -> IntList
iterate' f x = List (f x) (iterate' f (f x))

--repeat :: a -> [a]
repeat' : Int -> IntList
repeat' x = iterate' (\x : Int -> x) x

--replicate :: Int -> a -> [a]
replicate' : Int -> Int -> IntList
replicate' n x = if n == 0 then End else List x (replicate' (n-1) x)

--cycle :: [a] -> [a]
cycle' : IntList -> IntList
cycle' l = cycle' (plusplus l l)

--------------------

--take :: Int -> [a] -> [a]
take' : Int -> IntList -> IntList
take' i l =
    case l of {
        End -> End,
        List x rest -> 
            if i == 0
                then End
                else List x (take' (i-1) rest)
    }

--drop :: Int -> [a] -> [a]
drop' : Int -> IntList -> IntList
drop' i l =
    case l of {
        End -> End,
        List x rest -> 
            if i == 0
                then l
                else drop' (i-1) rest
    }

data ILTuple = ILT IntList IntList

--splitAt :: Int -> [a] -> ([a], [a])
splitAt' : Int -> IntList -> ILTuple
splitAt' n l = ILT (take' n l) (drop' n l)

--takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile' : (Int -> Bool) -> IntList -> IntList
takeWhile' f l =
    case l of {
        End -> End,
        List x rest -> 
            if f x
                then List x (takeWhile' f rest)
                else End
    }

--dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile' : (Int -> Bool) -> IntList -> IntList
dropWhile' f l =
    case l of {
        End -> End,
        List x rest -> 
            if f x
                then dropWhile' f rest
                else l
    }

--span :: (a -> Bool) -> [a] -> ([a], [a])
span' : (Int -> Bool) -> IntList -> ILTuple
span' f l = ILT (takeWhile' f l) (dropWhile' f l)

--break :: (a -> Bool) -> [a] -> ([a], [a])
break' : (Int -> Bool) -> IntList -> ILTuple
break' f l = span' (\x : Int -> not (f x)) l

--notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem' : Int -> IntList -> Bool
notElem' x l = 
    case l of {
        End -> True,
        List y rest -> 
            if x == y
                then False
                else notElem' x rest
    }

data ITuple = IT Int Int
data ITList = ITEnd | ITL ITuple ITList

--lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup' : Int -> ITList -> MaybeInt
lookup' x tl =
    case tl of {
        ITEnd -> Empty,
        ITL tuple rest -> 
            case tuple of {
                IT a b -> 
                    if x == a
                        then Number b
                        else lookup' x rest
            }
    }

--zip :: [a] -> [b] -> [(a, b)]
zip' : IntList -> IntList -> ITList
zip' l1 l2 =
    case l1 of {
        End -> ITEnd,
        List a rest1 ->
            case l2 of {
                End -> ITEnd,
                List b rest2 -> ITL (IT a b) (zip' rest1 rest2)
            }
    }

data ITuple3 = IT3 Int Int Int
data IT3List = IT3End | IT3L ITuple3 IT3List

--zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' : IntList -> IntList -> IntList -> IT3List
zip3' l1 l2 l3 =
    case l1 of {
        End -> IT3End,
        List a rest1 ->
            case l2 of {
                End -> IT3End,
                List b rest2 -> 
                    case l3 of {
                        End -> IT3End,
                        List c rest3 -> IT3L (IT3 a b c) (zip3' rest1 rest2 rest3)
                    }
            }
    }


--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' : (Int -> Int -> Int) -> IntList -> IntList -> IntList
zipWith' f l1 l2 =
    case l1 of {
        End -> End,
        List a rest1 ->
            case l2 of {
                End -> End,
                List b rest2 -> List (f a b) (zipWith' f rest1 rest2) 
            }
    }

--zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' : (Int -> Int -> Int -> Int) -> IntList -> IntList -> IntList -> IntList
zipWith3' f l1 l2 l3=
    case l1 of {
        End -> End,
        List a rest1 ->
            case l2 of {
                End -> End,
                List b rest2 -> 
                    case l3 of {
                        End -> End,
                        List c rest3 -> List (f a b c) (zipWith3' f rest1 rest2 rest3) 
                    }
            }
    }

--unzip :: [(a, b)] -> ([a], [b])
unzip' : ITList -> ILTuple
unzip' l = ILT (unzipA l) (unzipB l)

unzipA : ITList -> IntList
unzipA tl =
    case tl of {
        ITEnd -> End,
        ITL tuple rest ->
            case tuple of {
                IT a _ -> List a (unzipA rest)
            }
    }

unzipB : ITList -> IntList
unzipB tl =
    case tl of {
        ITEnd -> End,
        ITL tuple rest ->
            case tuple of {
                IT _ b -> List b (unzipB rest)
            }
    }

data IL3Tuple = IL3T IntList IntList IntList

--unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3' : IT3List -> IL3Tuple
unzip3' l = IL3T (unzipA' l) (unzipB' l) (unzipC' l)

unzipA' : IT3List -> IntList
unzipA' tl =
    case tl of {
        IT3End -> End,
        IT3L tuple rest ->
            case tuple of {
                IT3 a _ _ -> List a (unzipA' rest)
            }
    }

unzipB' : IT3List -> IntList
unzipB' tl =
    case tl of {
        IT3End -> End,
        IT3L tuple rest ->
            case tuple of {
                IT3 _ b _ -> List b (unzipB' rest)
            }
    }

unzipC' : IT3List -> IntList
unzipC' tl =
    case tl of {
        IT3End -> End,
        IT3L tuple rest ->
            case tuple of {
                IT3 _ _ c -> List c (unzipC' rest)
            }
    }

-- Strings

data Str = SEnd | Str Char Str
data StrList = SLEnd | StrL Str StrList

--lines :: String -> [String]
lines' : Str -> StrList
lines' str = lines'' (takeUntilf str isBreak) (dropUntilf str isBreak)

lines'' : Str -> Str -> StrList 
lines'' str1 str2 = 
    case str1 of {
        SEnd -> SLEnd,
        Str _ _ -> StrL str1 (lines' str2)
    }

isBreak : Char -> Bool
isBreak c = ((ord c) == (ord '\n'))

takeUntilf : Str -> (Char -> Bool) -> Str
takeUntilf str f =
    case str of {
        SEnd -> SEnd,
        Str c rest -> 
            if f c
                then SEnd
                else Str c (takeUntilf rest f)
    } 

dropUntilf : Str -> (Char -> Bool) -> Str
dropUntilf str f =
    case str of {
        SEnd -> SEnd,
        Str c rest -> 
            if f c
                then rest
                else dropUntilf rest f
    } 

--words :: String -> [String]
words' : Str -> StrList
words' str = words'' (takeUntilf str isSpace) (dropUntilf str isSpace)

words'' : Str -> Str -> StrList 
words'' str1 str2 = 
    case str1 of {
        SEnd -> SLEnd,
        Str _ _ -> StrL str1 (words' str2)
    }

isSpace : Char -> Bool
isSpace c = ((ord c) == (ord '\n')) || ((ord c) == (ord ' '))

--unlines :: [String] -> String
unlines' : StrList -> Str
unlines' sl =
    case sl of {
        SLEnd -> SEnd,
        StrL str rest -> 
            case rest of {
                SLEnd -> str,
                StrL _ _ -> concatStr (concatStr str (Str '\n' SEnd)) (unlines' rest)
            }
    }

concatStr : Str -> Str -> Str
concatStr s1 s2 =
    case s1 of {
        SEnd -> s2,
        Str c rest -> Str c (concatStr rest s2)
    }

--unwords :: [String] -> String
unwords' : StrList -> Str
unwords' sl =
    case sl of {
        SLEnd -> SEnd,
        StrL str rest -> 
            case rest of {
                SLEnd -> str,
                StrL _ _ -> concatStr (concatStr str (Str ' ' SEnd)) (unwords' rest)
            }
    }

list1 : IntList
list1 = makeNList 10

list2 : IntList
list2 = makeNList 2

main : Bool
main = 
    let a1 = map' (\x : Int -> x+1) (makeNList 2) in
    let a = equalL a1 (List 3 (List 2 End)) in

    let b1 = plusplus a1 a1 in
    let b = equalL b1 (List 3 (List 2 (List 3 (List 2 End)))) in

    let c1 = (List 5 (List 5 (List 2 (List 1 (List 5 End))))) in
    let c = equalL (filter' (\x:Int -> x==5) c1) (List 2 (List 1 End)) in

    let d = equalMaybe (safeHead' list1) 10 in

    let e = equalMaybe (safeLast' list1) 1 in

    let f = equalL (tail' list1) (makeNList 9) in

    let g1 = (List 1 (List 2 (List 3 End))) in
    let g = equalL (init' g1) (List 1 (List 2 End)) in

    let h = null' End && not (null' list1) in

    let i = (length' list1 == 10) && (length' (List 1 End) == 1) in
    
    let j1 = safeGetIndex' list1 5 in
    let j2 = safeGetIndex' list1 11 in
    let j = equalMaybe j1 5 && isEmpty j2 in

    let k1 = reverse' list1 in
    let k = equalL k1 
            (List 1 (List 2 (List 3 (List 4 (List 5 (List 6 (List 7 (List 8 (List 9 (List 10 End)))))))))) in

    let l1 = (\x : Int -> x > 0) in
    let l2 = (\x : Int -> x < 5) in
    let l = (andf' l1 list1) && not (andf' l2 list1) in

    let m1 = (\x : Int -> x == 5) in
    let m2 = (\x : Int -> x <= 0) in
    let m = (orf' m1 list1) && not (orf' m2 list1) in

    let n = (any' m1 list1) && not (any' m2 list1) in

    let o1 = (\x : Int -> x /= 0) in
    let o = (all' o1 list1) && not (all' m1 list1) in

    let p1 = LList list2 (LList list2 (LList list2 EEnd)) in
    let p2 = List 2 (List 1 (List 2 (List 1 (List 2 (List 1 End))))) in
    let p = equalL (concat' p1) p2 in 

    let q1 = (\x : Int -> List x (List x End)) in
    let q2 = List 2 (List 2 (List 1 (List 1 End))) in
    let q = equalL (concatMap' q1 list2) q2 in

    let r1 = (\x : Int -> (\y : Int -> x + y)) in
    let r2 = List 0 (List 2 (List 3 End)) in
    let r = equalL (scanl' r1 0 list2) r2 in

    let s = equalL (scanl1' r1 list2) (List 2 (List 3 End)) in

    let t = equalL (scanr' r1 0 list2) (List 3 (List 1 (List 0 End))) in

    let u = equalL (scanr1' r1 list2) (List 3 (List 1 End)) in

    --iterate
    --repeat

    let v = equalL (replicate' 2 10) (List 10 (List 10 End)) in

    --cycle

    let w = equalL (take' 3 list1)  (List 10 (List 9 (List 8 End))) in

    let x = equalL (drop' 8 list1) (List 2 (List 1 End)) in

    let y1 = splitAt' 5 list1 in
    let y2 = equalL (fst' y1) (List 10 (List 9 (List 8 (List 7 (List 6 End))))) in
    let y3 = equalL (snd' y1) (List 5 (List 4 (List 3 (List 2 (List 1 End))))) in
    let y = y2 && y3 in

    let z1 = (\x : Int -> x > 5) in
    let z = equalL (takeWhile' z1 list1) (List 10 (List 9 (List 8 (List 7 (List 6 End))))) in

    let aa = equalL (dropWhile' z1 list1) (List 5 (List 4 (List 3 (List 2 (List 1 End))))) in

    let ab1 = span' z1 list1 in
    let ab2 = equalL (fst' ab1) (List 10 (List 9 (List 8 (List 7 (List 6 End))))) in
    let ab3 = equalL (snd' ab1) (List 5 (List 4 (List 3 (List 2 (List 1 End))))) in
    let ab = ab2 && ab3 in

    let ac1 = break' z1 list1 in
    let ac2 = equalL (fst' ac1) End in
    let ac3 = equalL (snd' ac1) list1 in
    let ac = ac2 && ac3 in

    let ad = (notElem' 0 list1) && not (notElem' 1 list1) in

    let ae1 = ITL (IT 10 1) (ITL (IT 5 5) (ITL (IT 1 10) ITEnd)) in
    let ae = (equalMaybe (lookup' 1 ae1) 10) && (isEmpty (lookup' 20 ae1)) in

    let af = equalTL (zip' list1 list2) (ITL (IT 10 2) (ITL (IT 9 1) ITEnd)) in

    let ag = equalT3L (zip3' list1 list2 list2) (IT3L (IT3 10 2 2) (IT3L (IT3 9 1 1) IT3End)) in

    let ah1 = (\x : Int -> (\y : Int -> x - y)) in
    let ah = equalL (zipWith' ah1 list1 list2) (List 8 (List 8 End)) in

    let aj1 = (\x : Int -> (\y : Int -> (\z : Int -> x-(y-z)))) in
    let aj = equalL (zipWith3' aj1 list1 list1 list2) (List 2 (List 1 End)) in

    let ak1 = zip' list1 list2 in
    let ak2 = unzip' ak1 in
    let ak = equalL (fst' ak2) (List 10 (List 9 End)) && equalL (snd' ak2) (List 2 (List 1 End)) in

    let al1 = zip3' list1 list2 (makeNList 5) in
    let al2 = unzip3' al1 in
    let al3 = (List 10 (List 9 End)) in
    let al4 = (List 2 (List 1 End)) in
    let al5 = (List 5 (List 4 End)) in
    let al = equalL (fst'' al2) al3 && equalL (snd'' al2) al4 && equalL (trd'' al2) al5 in

    let am1 = Str 'o' (Str 'i' (Str '\n' (Str '.' SEnd))) in
    let am2 = StrL (Str 'o' (Str 'i' SEnd)) (StrL (Str '.' SEnd) SLEnd) in
    let am = equalStrL (lines' am1) am2 in

    let an1 = Str 'o' (Str 'i' (Str ' ' am1)) in
    let an2 = StrL (Str 'o' (Str 'i' SEnd)) am2 in
    let an = equalStrL (words' an1) an2 in

    let ao = equalStr am1 (unlines' am2) in

    let ap1 = Str 'o' (Str 'i' (Str ' ' (Str 'o' (Str 'i' (Str ' ' (Str '.' SEnd)))))) in
    let ap = equalStr ap1 (unwords' an2) in

    let result = a && b && c && d && e && f && g && h && j && k && l && m 
                && n && o && p && q && r && s && t && u && v && w && x && y
                && z && aa && ab && ac && ad && ae && af && ag && ah && aj 
                && ak && al && am && an && ao && ap in
    result

-- Auxiliary methods for main testing

equalMaybe : MaybeInt -> Int -> Bool
equalMaybe x b =
    case x of {
        Empty -> False,
        Number a -> a == b
    }

isEmpty : MaybeInt -> Bool
isEmpty n = case n of { Empty -> True, Number _ -> False }

equalL : IntList -> IntList -> Bool
equalL l1 l2 =
    case l1 of {
        End -> 
            case l2 of {
                End -> True,
                List _ _ -> False
            },
        List x rest1 ->
            case l2  of {
                End -> False,
                List y rest2 ->
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

equalTL : ITList -> ITList -> Bool
equalTL l1 l2 =
    case l1 of {
        ITEnd -> 
            case l2 of {
                ITEnd -> True,
                ITL _ _ -> False
            },
        ITL tuple1 rest1 -> 
            case l2 of {
                ITEnd -> False,
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

equalT3L : IT3List -> IT3List -> Bool 
equalT3L l1 l2 =
    case l1 of {
        IT3End -> 
            case l2 of {
                IT3End -> True,
                IT3L _ _ -> False
            },
        IT3L tuple1 rest1 -> 
            case l2 of {
                IT3End -> False,
                IT3L tuple2 rest2 -> 
                    if equalT3 tuple1 tuple2
                        then equalT3L rest1 rest2
                        else False 
            }
    }

makeNList : Int -> IntList
makeNList n = if n == 0 then End else List n (makeNList (n-1))

fst' : ILTuple -> IntList
fst' tuple = case tuple of { ILT first _ -> first}

snd' : ILTuple -> IntList
snd' tuple = case tuple of { ILT _ second -> second}

fst'' : IL3Tuple -> IntList
fst'' tuple = case tuple of { IL3T first _ _-> first}

snd'' : IL3Tuple -> IntList
snd'' tuple = case tuple of { IL3T _ second _ -> second}

trd'' : IL3Tuple -> IntList
trd'' tuple = case tuple of { IL3T _ _ third -> third}

equalStr : Str -> Str -> Bool
equalStr s1 s2 =
    case s1 of {
        SEnd ->
            case s2 of {
                SEnd -> True,
                Str _ _ -> False
            },
        Str c1 rest1 ->
            case s2 of {
                SEnd -> False,
                Str c2 rest2 -> 
                    if (ord c1) == (ord c2)
                        then equalStr rest1 rest2 
                        else False
            }
    }

equalStrL : StrList -> StrList -> Bool
equalStrL sl1 sl2 =
    case sl1 of {
        SLEnd ->
            case sl2 of {
                SLEnd -> True,
                StrL _ _ -> False
            },
        StrL s1 rest1 ->
            case sl2 of {
                SLEnd -> False,
                StrL s2 rest2 ->
                    if equalStr s1 s2
                        then equalStrL rest1 rest2
                        else False
            }
    }
