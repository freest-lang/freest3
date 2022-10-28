-- IntListChan cannot be *S for it is a choice whose components are not all *S
type IntListChan : *S = +{Nil: Skip, Cons: !Int ; IntListChan}
