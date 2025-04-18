data IntList = Nil | Cons Int IntList

type IntListC = +{NilC: Skip, ConsC: !Int;IntListC;?Int}

transform : IntList -> IntListC;a -> (IntList, a)
transform list c =
    case list of {
        Nil ->
            (Nil, select NilC c),
        Cons i rest ->
            let c = select ConsC c in
            let c = send i c in
            let (rest, c) = transform @(?Int ; a) rest c in
            let (y, c) = receive c in
            (Cons y rest, c)
    }

aCons, main : IntList

aCons = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))

main =
    let (l, c) = newHcClient @(IntListC;Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
    transform @Close aCons in
    close c;
    l
