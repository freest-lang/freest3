type InputStream = forall p:(bot,top) => ?p Int ; InputStream

filterStream : forall p:(bot,top), q:(p,top) => (Int ->[top,bot] Bool) ->[top,bot] InputStream ->[top,bot] dualof InputStream 1->[p,q+1] ()
filterStream =
    forall p:(bot,top), q:(p,top) =>
    \f: (Int ->[top,bot] Bool) ->
    \x: InputStream ->
    \y: dualof InputStream 1->
    let (v, x) = receive (x{p}) in -- priority: p
    if (f v) then
    let y = send v (y{q}) in     -- priority: q
    (filterStream{p+1}{q+1}) f x y
    else
    (filterStream{p+1}{q+1}) f x y

server : forall p:(bot,top) => dualof InputStream ->[top,p+1] ()
server =
    forall p:(bot,top) =>
    \x: dualof InputStream ->
    let x = send 2 (x{p}) in
    (server{p+1}) x

client : forall p:(bot,top) => InputStream ->[top,p+2] ()
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