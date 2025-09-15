type Point = +{ Move: !Int ; !Int; ?()
               , Read: ?Int ; ?Int
               } ; Point

pointClient : Point -> (Int,Int)
pointClient point =
    let point = select Read point in
    let (x, point) = receive point in
    let (y, point) = receive point in

    let point = select Move point in
    let point = send (x+1) point in
    let point = send (y+2) point in
    let (_, point) = receive point in

    let point = select Read point in
    let (x1, point) = receive point in
    let (y1, point) = receive point in
    (x1, y1)
    

pointServer : Int -> Int -> dualof Point -> Diverge
pointServer x1 y1 (Move point) = 
    let (x2, point) = receive point in
    let (y2, point) = receive point in
    let r = send () point in
    pointServer x2 y2 point
pointServer x1 y1 (Read point) =
    let point = send x1 point in
    let point = send y1 point in
    pointServer x1 y1 point

main : Int
main =
    let (p1, p2) = new @Point () in
    fork (\_:()1-> pointServer 0 0 p2);
    pointClient p1