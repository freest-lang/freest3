data List = Nil | Cons Int List

type Arrow = Int -> List
type ListSend : 1S = +{Cons: !Int;ListSend , Nil: Skip}
type ListComposed = Arrow -> ListSend


main : Int
main = 2
