data IntList = Nil | Cons Int IntList

type IntListS = &{NilC: Skip, ConsC: ?Int;IntListS;!Int}

listSum : IntListS ; a -> (Int,a)
listSum c =
    match c with {
        NilC c ->
            (0, c),
        ConsC c ->
            let (x, c) = receive c in
            let (rest, c) = listSum @(!Int ; a) c in
            let c = send (x + rest) c in
            (x+rest,c)
    }

aCons : IntList
aCons = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))

main : ()
main =
    let (_, c) = newHcServer @(IntListS ; Wait) ("127.0.0.1", "8081") |>
    listSum @Wait in
    wait c
