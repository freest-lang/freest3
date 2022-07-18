data IntList = Nil | Cons Int IntList
--type IntListC = +{Nil: Skip, Cons: !Int;IntListC;?Int}
--type IntListS = &{Nil: Skip, Cons: ?Int;IntListS;!Int}

transform : forall a : 1S . IntList -> (rec x: 1S. +{NilC: Skip, ConsC: !Int;x;?Int});a -> (IntList, a)
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


listSum : forall a : 1S . (rec x: 1S. &{NilC: Skip, ConsC: ?Int;x;!Int});a -> (Int,a)
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

aCons, main : IntList

aCons = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))

main =
    let (w, r) = new (rec x: 1S. +{NilC: Skip, ConsC: !Int;x;?Int}) in
    let _ = fork @(Int, Skip) $ listSum @Skip r in
    let (l, _) = transform @Skip aCons w in
    l
