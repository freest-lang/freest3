-- IntListChan cannot be SU for it is a choice (which is SL)
type IntListChan : SU = +{Nil: Skip, Cons: !Int ; IntListChan}

main : Int
main = 5
