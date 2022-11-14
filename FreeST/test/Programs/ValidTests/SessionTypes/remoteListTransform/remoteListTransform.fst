data IntList = Nil | Cons Int IntList
type IntListC : 1S = +{NilC: Skip, ConsC: !Int;IntListC;?Int}
type IntListS : 1S = &{NilC: Skip, ConsC: ?Int;IntListS;!Int}

transform : forall a : 1S . IntList -> IntListC;a -> (IntList, a)
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


listSum : forall a : 1S . IntListS;a -> (Int,a)
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
    let (w, r) = new @IntListC;End () in
    let _ = fork @() (\_:()1-> listSum @End r |> snd @Int @End |> close) in
    let (l, c) = transform @End aCons w in
    close c;
    l
