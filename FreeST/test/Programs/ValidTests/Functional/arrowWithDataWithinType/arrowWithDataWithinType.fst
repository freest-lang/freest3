data List = Nil | Cons Int List

type Arrow = Int -> List
type ListSend : 1S = +{ConsC: !Int;ListSend , NilC: Skip}
type ListComposed = Arrow -> ListSend


main : Int
main = 2
