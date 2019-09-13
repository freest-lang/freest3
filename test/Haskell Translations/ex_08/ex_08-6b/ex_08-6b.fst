-- VIII exercise 6b

data List = End | List Int List | ListC Char List
data Nbr = E | Nbr Int Nbr
data MaybeList = Empty | Only List

jogar : Nbr -> List -> Int -> MaybeList
jogar num list x =
    let jogada = jogar' num list x in
    if isCompleted jogada
        then Empty
        else Only jogada

jogar' : Nbr -> List -> Int -> List
jogar' num list x =
    case num of {
        E -> End,
        Nbr y rest1 -> 
            case list of {
                End -> End,
                List z rest2 -> List z (jogar' rest1 rest2 x),
                ListC c rest2 -> 
                    if x == y
                        then List x (jogar' rest1 rest2 x)
                        else ListC c (jogar' rest1 rest2 x)
            }
    }

isCompleted :  List -> Bool
isCompleted list =
    case list of {
        End -> True,
        List _ rest -> isCompleted rest,
        ListC _ _ -> False
    }

jogarJogo : Nbr -> List -> Nbr -> List
jogarJogo num l nums =
    case nums of {
        E -> ListC 'E' (ListC 'r' (ListC 'r' (ListC 'o' (ListC 'u' End)))),
        Nbr x rest -> 
            let jogada = jogar num l x in
            case jogada of {
                Empty -> ListC 'A' (ListC 'c' (ListC 'e' (ListC 'r' (ListC 't' (ListC 'o' (ListC 'u' End)))))),
                Only list -> jogarJogo num list rest
            }
    }

create : Int -> List
create n = if n == 0 then End else ListC '-' (create (n-1))

numberLength : Nbr -> Int
numberLength list =
    case list of {
        E -> 0,
        Nbr _ rest -> 1 + numberLength rest
    }

palavra : Nbr
palavra = Nbr 1 (Nbr 2 (Nbr 3 E))

guesses : Nbr
guesses = Nbr 3 (Nbr 1 (Nbr 0 E))

main : List
main = jogarJogo palavra (create (numberLength palavra)) guesses
--result = ListC 'E' (ListC 'r' (ListC 'r' (ListC 'o' (ListC 'u' End))))