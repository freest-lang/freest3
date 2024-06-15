data List = Cons Int List | Nil

type ListC = &{NilC: Wait, ConsC: ?Int ; ListC}

read : ListC -> List
read (ConsC c) =
  let (x, c) = receive c in
  Cons x (read c)
read (NilC c) = wait c ; Nil

write : List -> dualof ListC -> ()
write (Cons x xs) c =
  c |> select ConsC
    |> send x
    |> write xs
write Nil c =
  c |> select NilC
    |> close

aList, main : List

aList = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
main = forkWith @ListC @() (write aList) |> read

