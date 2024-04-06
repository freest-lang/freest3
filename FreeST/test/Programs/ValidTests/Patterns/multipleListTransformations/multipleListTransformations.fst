data IntList = Nil | Cons Int IntList

-- sum' +
sum' : IntList -> Int
sum' Nil        = 0
sum' (Cons x y) = x + sum' y

toNegative : IntList -> IntList
toNegative Nil        = Nil
toNegative (Cons x y) = Cons (-x) (toNegative y)

-- sumNegatives -
sumNegatives : IntList -> Int
sumNegatives l = sum' (toNegative l)

-- specInterchange (specific)
specInterchange : IntList -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> Int
specInterchange Nil                 _  _  = 0
specInterchange (Cons a  Nil)       f1 f2 = a
specInterchange (Cons a (Cons c d)) f1 f2 = specInterchange (Cons (f1 a c) d) f2 f1

-- repetidor
repetidor : Int -> Int -> IntList
repetidor i n =
    if i == 0 
        then Nil
        else Cons n (repetidor (i-1) n)

-- makeUntil
makeUntil : Int -> IntList
makeUntil i = if i == 0 then Nil else Cons i (makeUntil (i-1)) 

-- takeOutNumber
takeOutNumber : IntList -> Int -> IntList
takeOutNumber Nil        n = Nil
takeOutNumber (Cons x y) n = if x == n
                                then takeOutNumber y n
                                else Cons x (takeOutNumber y n)

-- incrementList
incrementList : IntList -> Int -> IntList
incrementList Nil        i = Nil
incrementList (Cons x y) i = Cons (x+i) (incrementList y i)

-- multiplyList
multiplyList : IntList -> Int -> IntList
multiplyList Nil        m = Nil
multiplyList (Cons x y) m = Cons (x*m) (multiplyList y m)

-- decrementList
decrementList : IntList -> Int -> IntList
decrementList Nil        i = Nil
decrementList (Cons x y) i = Cons (x-i) (decrementList y i)

-- divideList
divideList : IntList -> Int -> IntList
divideList Nil        m = Nil
divideList (Cons x y) m = Cons (x/m) (divideList y m)

equalsList : IntList -> IntList -> Bool
equalsList Nil        Nil        = True
equalsList (Cons a b) (Cons c d) = a == c && equalsList b d
equalsList _          _          = False

list1 : IntList
list1 = repetidor 10 2

list2 : IntList
list2 = makeUntil 10

main : Bool
main = 
    let a = sum' list1 in
    let b = sumNegatives list1 in
    let c = specInterchange list1 (+) (-) in
    let d = specInterchange list1 (*) (div) in
    let e = takeOutNumber list2 10 in
    let f = decrementList (incrementList list1 1) 1 in
    let g = divideList (multiplyList list1 2) 2 in
    (a == 10*2) && (b == 10*-2) && (c == 4) && (d == 4) && (equalsList (makeUntil 9) e) && (equalsList list1 f) && (equalsList list1 g)