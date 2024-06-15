module List where

-- TODO: (++)
(++) : [Int] -> [Int] -> [Int]
[]      ++ ys = ys
(x::xs) ++ ys = x :: (xs ++ ys) 

head : [Int] -> Int
head []       = error @Int "*** List.head: empty list"
head (x :: _) = x

last : [Int] -> Int
last []        = error @Int "*** List.last: empty list"
last (x :: []) = x
last (_ :: xs) = last xs

tail : [Int] -> [Int]
tail []        = error @[Int] "*** List.tail: empty list"
tail (_ :: []) = []
tail (_ :: xs) = xs

init : [Int] -> [Int]
init []        = error @[Int] "*** List.init: empty list"
init (_ :: []) = []
init (x::xs)   = x :: init xs

-- uncons :: [a] -> Maybe (a, [a])

singleton : Int -> [Int]
singleton x = [x]

null : [Int] -> Bool
null [] = True
null _  = False

length : [Int] -> Int
length []      = 0
length (_::xs) = 1 + length xs

-- Reducing lists (folds)

foldl : forall a:*T . (a -> Int -> a) -> a -> [Int] -> a
foldl f z []      = z                  
foldl f z (x::xs) = foldl @a f (f z x) xs

foldr : forall a:*T . (Int -> a -> a) -> a -> [Int] -> a
foldr f z []      = z 
foldr f z (x::xs) = f x (foldr @a f z xs) 


-- List transformations

map : (Int -> Int) -> [Int] -> [Int]
map _ []      = []
map f (x::xs) = f x :: map f xs

reverse : [Int] -> [Int]
reverse = foldl @[Int] (\acc:[Int] x:Int -> x::acc) [] 

intersperse : Int -> [Int] -> [Int]
intersperse _ []      = []
intersperse v (x::[]) = [x]
intersperse v (x::xs) = x :: v :: intersperse v xs

-- intercalate : forall a:*T . [a] -> [[a]] -> [a]

-- transpose : forall a:*T . [[a]] -> [[a]]

-- subsequences : forall a:*T . [a] -> [[a]]

-- permutations : forall a:*T . [a] -> [[a]]

-- Special folds

-- Functions that do not make sense for now. Not applicable for lists of integers
-- concat, concatMap, and, or,

any : (Int -> Bool) -> [Int] -> Bool
any _ []      = False 
any f (x::xs) = f x || any f xs

all : (Int -> Bool) -> [Int] -> Bool
-- all f xs = foldr (&&) True (map f xs)
all _ []      = True 
all f (x::xs) = f x && all f xs

concatMap : (Int -> [Int]) -> [Int] -> [Int]
concatMap _ []      = []
concatMap f (x::xs) = f x ++ concatMap f xs


sum : [Int] -> Int
sum []      = 0
sum (x::xs) = x + sum xs

product : [Int] -> Int
product []      = 1
product (x::xs) = x * product xs

maximum : [Int] -> Int
maximum []      = error @Int "*** List.maximum: empty list"
maximum (x::xs) = foldl @Int (\acc:Int y:Int -> if acc > y then acc else y) x xs


minimum : [Int] -> Int
minimum []      = error @Int "*** List.minimum: empty list"
minimum (x::xs) = foldl @Int (\acc:Int y:Int -> if acc > y then y else acc) x xs

-- Building lists

-- Scans

-- Note: Code taken from Haskell prelude
-- Note: last (scanl f z xs) == foldl f z xs.
-- scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
scanl : (Int -> Int -> Int) -> Int -> [Int] -> [Int]
scanl _ q []      = [q]
scanl f q (x::xs) = q :: scanl f (f q x) xs


scanl1 : (Int -> Int -> Int) -> [Int] -> [Int]
scanl1 _ []           = [] 
scanl1 f (x::[])      = [x]
scanl1 f (x::(y::xs)) = x :: scanl f (f x y) xs 

-- scanr f z [..., x1, x2] == [...,  x1 `f` (x2 `f` z) , x2 `f` z, z]

scanr : (Int -> Int -> Int) -> Int -> [Int] -> [Int]
scanr _ x []      = [x]
scanr f x (y::ys) =
  let res = scanr f x ys in (f y (head res)) :: res


scanr1 : (Int -> Int -> Int) -> [Int] -> [Int]
scanr1 _ []      = []
scanr1 _ (x::[]) = [x]
scanr1 f (x::xs) =
  let res = scanr1 f xs in f x (head res) :: res 


-- Accumulating maps

mapAccumL : forall a:*T . (a -> Int -> (a, Int)) -> a -> [Int] -> (a, [Int])
mapAccumL _ s []      = (s, [])
mapAccumL f s (z::zs) =
  let (x,y) = f s z in
  let (acc, ys) = mapAccumL @a f x zs in
  (acc, y :: ys)

mapAccumR : forall a:*T . (a -> Int -> (a, Int)) -> a -> [Int] -> (a, [Int])
mapAccumR _ s []      = (s, [])
mapAccumR f s (z::zs) =
  let (acc, ys) = mapAccumR @a f s zs in
  let (x,y) = f acc z in
  (x, y::ys)


-- Sublists

take : Int -> [Int] -> [Int]
take _ []      = []
take n (x::xs) 
  | n <= 0    = []
  | otherwise = x :: take (n-1) xs

drop : Int -> [Int] -> [Int]
drop _ []      = []
drop n (x::xs)
  | n <= 0    = x::xs
  | otherwise = drop (n-1) xs

splitAt : Int -> [Int] -> ([Int], [Int])
splitAt _ []      = ([],[])
splitAt n (x::xs)
  | n <= 0    = ([],x::xs)
  | otherwise = 
    let (ys,zs) = splitAt (n-1) xs in
    (x::ys,zs)
   
takeWhile : (Int -> Bool) -> [Int] -> [Int] 
takeWhile _ []      = []
takeWhile p (x::xs)
  | p x       = x :: takeWhile p xs
  | otherwise = []

dropWhile : (Int -> Bool) -> [Int] -> [Int]
dropWhile _ []      = []
dropWhile p (x::xs)
  | p x       = dropWhile p xs
  | otherwise = x::xs

span : (Int -> Bool) -> [Int] -> ([Int], [Int])
span _ []      =  ([], [])
span p (x::xs)
  | p x       = let (ys,zs) = span p xs in (x::ys,zs)
  | otherwise = ([],x::xs)
    
break : (Int -> Bool) -> [Int] -> ([Int], [Int])
break p = span (\x : Int -> not (p x))
-- break _ []            =  ([], [])
-- break p (x::xs)
--   | p x       = ([],x::xs)
--   | otherwise = let (ys,zs) = break p xs in (x::ys,zs)


  
-- Searching lists

-- Searching by equality

elem : Int -> [Int] -> Bool
elem x xs = any (== x) xs

notElem : Int -> [Int] -> Bool
notElem x xs = not $ elem x xs

filter : (Int -> Bool) -> [Int] -> [Int]
filter p = foldr @[Int] (\x:Int acc:[Int] -> if p x then x :: acc else acc) [] 

partition : (Int -> Bool) -> [Int] -> ([Int], [Int])
partition _ []      = ([],[]) 
partition p (x::xs) =
  let (ys,zs) = partition p xs in
  if p x then (x::ys,zs) else (ys,x::zs)

--lookup :: Eq a => a -> [(a, b)] -> Maybe b


-- Indexing lists


-- TODO:
-- (!!) : [Int] -> Int -> Int
-- (!!) xs x =
--   error @Int "*** List.head: empty list"

-- s     !! n | n < 0 =  error "Prelude.!!: negative index"
-- []     !! _         =  error "Prelude.!!: index too large"
-- (x:_)  !! 0         =  x
-- (_:xs) !! n         =  xs !! (n-1)

nth : [Int] -> Int -> Int
nth [] _ =  error @Int "*** List.getAtIndex: index too large"
nth (x::xs) n
  | n <  0    = error @Int "*** List.getAtIndex: negative index"
  | n == 0    = x
  | otherwise = nth xs (n-1)


-- elemIndex :: Eq a => a -> [a] -> Maybe Int
-- elemIndices :: Eq a => a -> [a] -> [Int]
-- findIndex :: (a -> Bool) -> [a] -> Maybe Int
-- findIndices :: (a -> Bool) -> [a] -> [Int]



-- Zipping and unzipping lists

--zip :: [a] -> [b] -> [(a, b)]

--zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith : (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
zipWith _ []      _       = []
zipWith _ _       []      = []
zipWith f (x::xs) (y::ys) = f x y :: zipWith f xs ys

--zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 : (Int -> Int -> Int -> Int) -> [Int] -> [Int] -> [Int] -> [Int]
zipWith3 _ []      _       _       = []
zipWith3 _ _       []      _       = []
zipWith3 _ _       _       []      = []
zipWith3 f (x::xs) (y::ys) (z::zs) = f x y z :: zipWith3 f xs ys zs

--unzip :: [(a, b)] -> ([a], [b])

--unzip3 :: [(a, b, c)] -> ([a], [b], [c])



-- Extra functions not present in Haskell's Prelude/Data.List


--(!!) :: [a] -> Int -> a
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
elemAt : [Int] -> Int -> Int
elemAt []      n 
  | n < 0     = error @Int "*** Exception: Prelude.elemAt: negative index"
  | otherwise = error @Int "*** Exception: Prelude.elemAt: index too large"
elemAt (x::xs) n 
  | n == 0    = x
  | otherwise = elemAt xs (n - 1)

-- TODO: polymorphic (==)
equal : [Int] -> [Int] -> Bool
equal []      []      = True
equal []      _       = False
equal _       []      = False
equal (x::xs) (y::ys) = x == y && equal xs ys
