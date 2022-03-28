-- V exercise 3e

pow2 : Int -> Int
pow2 x = x*x

map' : ([Int] -> [Int]) -> [[Int]] -> [[Int]]
map' f list =
    case list of {
        [] -> [],
        intList :: rest -> (f intList) :: (map' f rest)
    }

-- becase there's no polymorphic datas there's the need to make this method
map'' : (Int -> Int) -> [Int] -> [Int]
map'' f list =
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map'' f rest)
    }

{-
map : forall a,b => (a -> a) -> b -> b
map f list =
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map1 f rest)
    }

    In this case we have define LEnd and LList, making it impossible to abstract,
    There's also the problem that we cannot say that b is made of multiple a's

-}

list1 : [Int]
list1 = [1,2]
list2 : [Int]
list2 = [3,4,5]

list : [[Int]]
list = [list1,list2]

main : IntListList
main = map' (map'' pow2) list
-- result = [[1,4],[9,16,25]]