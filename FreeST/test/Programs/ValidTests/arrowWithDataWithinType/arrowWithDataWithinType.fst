data List = Nil | Cons Int List

type Arrow = Int -> List
type ListSend : SL = +{Cons: !Int;ListSend , Nil: Skip}
type ListComposed = Arrow -> ListSend


main : Int
main = 2
