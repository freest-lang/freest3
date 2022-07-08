-- VIII exercise 6a

data List = End | List Int List | ListC Char List
data Nbr = E | Nbr Int Nbr
data MaybeList = Empty | Only List

jogar : Nbr -> List -> Int -> MaybeList
jogar num list x =
    if isCompleted list
        then Empty
        else Only (jogar' num list x)

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

main : MaybeList
main = jogar (Nbr 1 (Nbr 2 (Nbr 3 E))) (ListC '-' (ListC '-' (ListC '-' End))) 1
--result = Only (List 1 (ListC '-' (ListC '-' End)))
