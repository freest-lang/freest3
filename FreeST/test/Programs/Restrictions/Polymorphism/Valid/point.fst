type Point = forall a:(bot,top), b:(a,top), c:(a,top) => 
                                +a{ Move: !b Int ; !b+1 Int; ?b+2()
                                  , Read: ?c Int ; ?c+1 Int
                                  } ; Point

pointClient : forall a:(bot,top), b:(a,top), c:(a,top) => Point ->[top,c+7+1] (Int,Int)
pointClient =
    forall a:(bot,top), b:(a,top), c:(a,top) =>
    \point: Point ->
    let point = select Read (point{a}{b}{c}) in
    let (x, point) = receive point in
    let (y, point) = receive point in
    
    let point = select Move (point{a+3}{b}{c}) in
    let point = send (x+1) point in
    let point = send (y+2) point in
    let (_, point) = receive point in

    let point = select Read (point{a+7}{b}{c+7}) in
    let (x1, point) = receive point in
    let (y1, point) = receive point in

    (pointClient{a+1}{b+1}{c+1}) point
    

pointServer : forall a:(bot,top), b:(a,top), c:(a,top) => Int ->[top,bot] Int ->[top,bot] dualof Point ->[top,b+2] Diverge
pointServer =
    forall a:(bot,top), b:(a,top), c:(a,top) =>
    \x1: Int ->
    \y1: Int ->
    \point: dualof Point ->  
    match (point{a}{b}{c}) with {
        Move point ->
            let (x2, point) = receive point in
            let (y2, point) = receive point in
            let point = send () point in
            (pointServer{a+1}{b+1}{c+1}) x2 y2 point,
        Read point ->
            let point = send x1 point in
            let point = send y1 point in
            (pointServer{a+1}{b+1}{c+1}) x1 y1 point
    }

main : (Int,Int)
main =
    let (p1, p2) = new @Point () in
    fork (\_:()1-> (pointServer{1}{5}{2}) 0 0 p2);
    (pointClient{1}{5}{2}) p1