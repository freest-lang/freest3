-- IntListChan cannot be SU for it is a choice (which is SL)
type IntListChan : *S = +{Nil: Skip, Cons: !Int ; IntListChan}

main : Int
main = 5
