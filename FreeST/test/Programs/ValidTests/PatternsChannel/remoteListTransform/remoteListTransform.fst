data IntList = Nil | List Int IntList
--type IntListC = +{Nil: Skip, List: !Int;IntListC;?Int}
--type IntListS = &{Nil: Skip, List: ?Int;IntListS;!Int}

transform : forall a : 1S . IntList -> (rec x:1S. +{NilC: Skip, ListC: !Int;x;?Int});a -> (IntList, a)
transform Nil           c = (Nil, select NilC c)
transform (List i rest) c = 
    let c = select ListC c in
    let c = send i c in
    let (rest, c) = transform@(?Int;a) rest c in
    let (y, c) = receive c in
    (List y rest, c)


listSum : forall a : 1S . (rec x:1S. &{NilC: Skip, ListC: ?Int;x;!Int});a -> (Int,a)
listSum (NilC  c) = (0, c)
listSum (ListC c) = 
    let (x, c) = receive c in
    let (rest, c) = listSum@(!Int;a) c in
    let c = send (x + rest) c in
    (x+rest,c)

aList : IntList
aList = List 5 (List 4 (List 3 (List 2 (List 1 Nil))))
    
main : IntList
main =
    let (w, r) = new (rec x:1S. +{NilC: Skip, ListC: !Int;x;?Int}) in
    fork (\_:() 1-> listSum@Skip r) ;
    let (l, _) = transform@Skip aList w in
    l
