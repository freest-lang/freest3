type InputStream = forall p:(bot,top) => ?p Int ; Wait p+2

filterStream : forall a:(bot,top), b:(a,top) => (Int ->[top,bot] Bool) ->[top,bot] InputStream ->[top,bot] dualof InputStream 1->[a,b+2] ()
filterStream =
    forall a:(bot,top), b:(a,top) =>
    \f: (Int ->[top,bot] Bool) ->
    \x: InputStream ->
    \y: dualof InputStream 1->
    let (v, x) = receive (x{a}) in
    -- if (f v) then
    let y = send v (y{b}) in
    wait x;
    close y
    -- else
    --     wait x;
    --     close y

server : forall b:(bot,top) => dualof InputStream ->[top,b+2] ()
server =
    forall b:(bot,top) =>
    \x: dualof InputStream ->
    let x = send 2 (x{b}) in
    close x

client : forall a:(bot,top) => InputStream ->[top,a+2] ()
client =
    forall a:(bot,top) =>
    \y: InputStream ->
    let (v, y) = receive (y{a}) in
    wait y

main : ()
main =
    let (r1, w1) = new @InputStream () in
    let (r2, w2) = new @InputStream () in
    fork (\_ : () 1-> (server{1}) w1);
    fork (\_ : () 1-> (client{2}) r2);
    (filterStream{1}{2}) (\x : Int -> x == 2) r1 w2