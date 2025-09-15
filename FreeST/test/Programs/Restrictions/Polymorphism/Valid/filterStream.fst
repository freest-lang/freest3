type InputStream = forall p:(bot,top) => ?p Int ; InputStream

filterStream : forall a:(bot,top), b:(a,top) => (Int ->[top,bot] Bool) ->[top,bot] InputStream ->[top,bot] dualof InputStream 1->[a,b] ()
filterStream =
    forall a:(bot,top), b:(a,top) =>
    \f: (Int ->[top,bot] Bool) ->
    \x: InputStream ->
    \y: dualof InputStream 1->
    let (v, x) = receive (x{a}) in -- priority: p
    if (f v) then
        let y = send v (y{b}) in     -- priority: q
        (filterStream{a+1}{b+1}) f x y
    else
        (filterStream{a+1}{b+1}) f x y

server : forall p:(bot,top) => dualof InputStream ->[top,p] ()
server =
    forall p:(bot,top) =>
    \x: dualof InputStream ->
    let x = send 2 (x{p}) in
    (server{p+1}) x

client : forall p:(bot,top) => InputStream ->[top,p] ()
client =
    forall p:(bot,top) =>
    \y: InputStream ->
    let (v, y) = receive (y{p}) in
    (client{p+1}) y

main : ()
main =
    let (r1, w1) = new @InputStream () in
    let (r2, w2) = new @InputStream () in
    fork (\_ : () 1-> (server{1}) w1);
    fork (\_ : () 1-> (client{2}) r2);
    (filterStream{1}{2}) (\x : Int -> x == 2) r1 w2