data IntList = End | List Int IntList
--type IntListC = +{End: Skip, List: !Int;IntListC;?Int}
--type IntListS = &{End: Skip, List: ?Int;IntListS;!Int}

transform : forall a : SL . IntList -> (rec x:SL. +{EndC: Skip, ListC: !Int;x;?Int});a -> (IntList, a)
transform list c =
    case list of {
        End ->
            (End, select EndC c),
        List i rest ->
            let c = select ListC c in
            let c = send i c in
            let (rest, c) = transform[?Int;a] rest c in
            let (y, c) = receive c in
            (List y rest, c)
    }


listSum : forall a : SL . (rec x:SL. &{EndC: Skip, ListC: ?Int;x;!Int});a -> (Int,a)
listSum c =
    match c with {
        EndC c ->
            (0, c),
        ListC c ->
            let (x, c) = receive c in
            let (rest, c) = listSum[!Int;a] c in
            let c = send (x + rest) c in
            (x+rest,c)
    }

aList : IntList
aList = List 5 (List 4 (List 3 (List 2 (List 1 End))))

main : IntList
main =
    let (w, r) = new (rec x:SL. +{EndC: Skip, ListC: !Int;x;?Int}) in
    let _ = fork[(Int, Skip)] $ listSum[Skip] r in
    let (l, _) = transform[Skip] aList w in
    l
