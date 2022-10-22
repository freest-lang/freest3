data List = Nil | Cons Int List

data Maybe = Nothing | Just Int

--map :: (a -> b) -> [a] -> [b]
map : (Int -> Int) -> List -> List
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

--(++) :: [a] -> [a] -> [a]
append : List -> List -> List
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

--filter :: (a -> Bool) -> [a] -> [a]
filter : (Int -> Bool) -> List -> List
filter _ Nil = Nil
filter p (Cons x xs) | p x =  filter p xs
                     | otherwise = Cons x (filter p xs)

--head :: [a] -> a
head : List -> Int
head (Cons x _) = x

--last :: [a] -> a
last : List -> Int
last (Cons x Nil) = x
last (Cons _ xs) = last xs

--tail :: [a] -> [a]
tail : List -> List
tail (Cons _ xs) = xs

--init :: [a] -> [a]
init : List -> List
init (Cons _ Nil) = Nil
init (Cons x xs) = Cons x (init xs)

--null :: Foldable t => t a -> Bool
null : List -> Bool
null Nil = True
null (Cons _ _ ) = False

--length :: Foldable t => t a -> Int
length : List -> Int
length Nil = 0
length (Cons _ xs) = 1 + length xs

--(!!) :: [a] -> Int -> a
-- TODO: Fix
{-
>>> ['a', 'b', 'c'] !! 0
'a'
>>> ['a', 'b', 'c'] !! 2
'c'
>>> ['a', 'b', 'c'] !! 3
*** Exception: Prelude.!!: index too large
>>> ['a', 'b', 'c'] !! (-1)
*** Exception: Prelude.!!: negative index
-}
elemAt : List -> Int -> Maybe
elemAt l i = 
    case l of {
        Nil -> Nothing,
        Cons x xs' ->
            if i == 0
                then Just x
                else elemAt xs' (i-1)
    }

--reverse :: [a] -> [a]
reverse : List -> List
reverse = rev Nil
-- where
rev : List -> List -> List
rev xs Nil = xs
rev xs (Cons y ys) = rev (Cons y xs) ys

--all :: Foldable t => (a -> Bool) -> t a -> Bool
all : (Int -> Bool) -> List -> Bool
all p xs =
    case xs of {
        Nil -> True,
        Cons x xs' ->
            if p x 
                then all p xs'
                else False
    } 

--any :: Foldable t => (a -> Bool) -> t a -> Bool
any : (Int -> Bool) -> List ->  Bool
any p xs =
    case xs of {
        Nil -> False,
        Cons x xs' -> 
            if p x
                then True
                else any p xs'
    }

--concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap : (Int -> List) -> List -> List
concatMap f xs =
    case xs of {
        Nil -> Nil,
        Cons x xs' -> append (f x) (concatMap f xs')
    }


-- Note: last (scanl f z xs) == foldl f z xs.

--scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl : (Int -> Int -> Int) -> Int -> List -> List
scanl f acc l = Cons acc (scan f acc l)

scan : (Int -> Int -> Int) -> Int -> List -> List
scan f acc l = 
    case l of {
        Nil -> Nil,
        Cons x xs' -> Cons (f x acc) (scan f (f x acc) xs')
    }

--scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 : (Int -> Int -> Int) -> List -> List
scanl1 f l =
    case l of {
        Nil -> Nil,
        Cons x xs' -> Cons x (scan f x xs')
    }

--scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr : (Int -> Int -> Int) -> Int -> List -> List
scanr f acc l = reverse (scanl f acc (reverse l))

--scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 : (Int -> Int -> Int) -> List -> List
scanr1 f l = reverse (scanl1 f (reverse l))

--take :: Int -> [a] -> [a]
take : Int -> List -> List
take i l =
    case l of {
        Nil -> Nil,
        Cons x xs' -> 
            if i == 0
                then Nil
                else Cons x (take (i-1) xs')
    }

--drop :: Int -> [a] -> [a]
drop : Int -> List -> List
drop i l =
    case l of {
        Nil -> Nil,
        Cons x xs' -> 
            if i == 0
                then l
                else drop (i-1) xs'
    }

--splitAt :: Int -> [a] -> ([a], [a])
splitAt : Int -> List -> (List, List)
splitAt n l = (take n l, drop n l)

--Takewhile :: (a -> Bool) -> [a] -> [a]
takeWhile : (Int -> Bool) -> List -> List
takeWhile f l =
    case l of {
        Nil -> Nil,
        Cons x xs' -> 
            if f x
                then Cons x (takeWhile f xs')
                else Nil
    }

--dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile : (Int -> Bool) -> List -> List
dropWhile f l =
    case l of {
        Nil -> Nil,
        Cons x xs' -> 
            if f x
                then dropWhile f xs'
                else l
    }

--span :: (a -> Bool) -> [a] -> ([a], [a])
span : (Int -> Bool) -> List -> (List, List)
span f l =(takeWhile f l, dropWhile f l)

--break :: (a -> Bool) -> [a] -> ([a], [a])
break : (Int -> Bool) -> List -> (List, List)
break f l = span (\x : Int -> not (f x)) l

--elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem : Int -> List -> Bool
elem _ Nil = False
elem x (Cons y ys) | x == y = True
                   | otherwise = elem x ys
                   
--notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem' : Int -> List -> Bool
notElem' x xs = not $ elem x xs

data ITuple = IT Int Int
data ITCons = ITNil | ITL ITuple ITCons

--lookup :: Eq a => a -> [(a, b)] -> Maybe b

--zip :: [a] -> [b] -> [(a, b)]

data ITuple3 = IT3 Int Int Int
data IT3Cons = IT3Nil | IT3L ITuple3 IT3Cons

--zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith : (Int -> Int -> Int) -> List -> List -> List
zipWith f l1 l2 =
    case l1 of {
        Nil -> Nil,
        Cons a xs'1 ->
            case l2 of {
                Nil -> Nil,
                Cons b xs'2 -> Cons (f a b) (zipWith f xs'1 xs'2) 
            }
    }

--zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 : (Int -> Int -> Int -> Int) -> List -> List -> List -> List
zipWith3 f l1 l2 l3=
    case l1 of {
        Nil -> Nil,
        Cons a xs'1 ->
            case l2 of {
                Nil -> Nil,
                Cons b xs'2 -> 
                    case l3 of {
                        Nil -> Nil,
                        Cons c xs'3 -> Cons (f a b c) (zipWith3 f xs'1 xs'2 xs'3) 
                    }
            }
    }

--unzip :: [(a, b)] -> ([a], [b])

--unzip3 :: [(a, b, c)] -> ([a], [b], [c])
