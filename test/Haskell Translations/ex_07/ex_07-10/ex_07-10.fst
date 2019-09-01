-- VII exercise 10

data Set = End | Set Int Set

insertSet : Int -> Set -> Set
insertSet x set =
    case set of {
        End -> Set x End,
        Set y rest ->
            if x == y
                then set
                else 
                    if x < y
                        then Set x set
                        else Set y (insertSet x rest)
        }

equals : Set -> Set -> Bool
equals s1 s2 =
    case s1 of {
        End -> case s2 of { End -> True, Set _ _ -> False },
        Set x r1 -> 
            case s2 of {
                End -> False,
                Set y r2 -> 
                    if x /= y 
                        then False
                        else equals r1 r2
            }
    }

data Showable = B Char Showable | Item Int Char Showable | E

showSet : Set -> Showable
showSet set = B '{' (showSet' set)

showSet' : Set -> Showable
showSet' set =
    case set of {
        End -> E,
        Set x rest -> 
            case rest of {
                End -> Item x '}' E,
                Set _ _ -> Item x ',' (showSet' rest) 
            }
    }

main : Showable
main = 
    let set = insertSet 8 (insertSet 9 (insertSet 4 (insertSet 2 (insertSet 5 End)))) in
    let b1 = equals set set                                     in -- True
    let b2 = not (equals set (insertSet 10 set))                in -- True
    let b = b1 && b2                                            in -- True
    showSet set
--result = B '{' (Item 2 ',' (Item 4 ',' (Item 5 ',' (Item 8 ',' (Item 9 '}' E)))))