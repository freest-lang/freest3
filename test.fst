data Nil = | Nil
type NilC : 1S = &{Nil: End}

read : NilC -> ()
read (NilC c) = ()

write : Nil -> ()
write Nil c = ()

main : ()
main = ()
