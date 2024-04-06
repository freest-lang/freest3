-- VIII exercise 6b

data List = Nil | List Int List | ListC Char List
data Nbr = E | Nbr Int Nbr
data MaybeList = Empty | Only List

jogar' : Nbr -> List -> Int -> List
jogar' E             list            x = Nil
jogar' (Nbr y rest1) Nil             x = Nil
jogar' (Nbr y rest1) (List  z rest2) x = List z (jogar' rest1 rest2 x)
jogar' (Nbr y rest1) (ListC c rest2) x 
    | x == y    = List  x (jogar' rest1 rest2 x)
    | otherwise = ListC c (jogar' rest1 rest2 x)

isCompleted :  List -> Bool
isCompleted Nil            = True
isCompleted (List  _ rest) = isCompleted rest
isCompleted (ListC _ _)    = False

jogar : Nbr -> List -> Int -> MaybeList
jogar num list x =
    let jogada = jogar' num list x in
    if isCompleted jogada
        then Empty
        else Only jogada

jogarJogo' : Nbr -> List -> Nbr -> MaybeList -> List
jogarJogo' num l rest Empty       = ListC 'A' (ListC 'c' (ListC 'e' (ListC 'r' (ListC 't' (ListC 'o' (ListC 'u' Nil))))))
jogarJogo' num l rest (Only list) = jogarJogo num list rest

and jogarJogo : Nbr -> List -> Nbr -> List
jogarJogo num l E = ListC 'E' (ListC 'r' (ListC 'r' (ListC 'o' (ListC 'u' Nil))))
jogarJogo num l (Nbr x rest) = jogarJogo' num l rest $ jogar num l x

create : Int -> List
create n | n == 0 = Nil | otherwise = ListC '-' (create (n-1))

numberLength : Nbr -> Int
numberLength E            = 0
numberLength (Nbr _ rest) = 1 + numberLength rest

palavra : Nbr
palavra = Nbr 1 (Nbr 2 (Nbr 3 E))

guesses : Nbr
guesses = Nbr 3 (Nbr 1 (Nbr 0 E))

main : List
main = jogarJogo palavra (create (numberLength palavra)) guesses
--result = ListC 'E' (ListC 'r' (ListC 'r' (ListC 'o' (ListC 'u' Nil))))
