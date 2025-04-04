type X = !2();Close 1
type Y = !3();Close 4

server : dualof X ->[top,bot] Y 1->[1,3] ()
server x y = 
    let (_,x) = receive x in
    let y = send () y in
    wait x;
    close y

client : X ->[top,bot] dualof Y 1->[1,3] ()
client x y = 
    let (_,y) = receive y in
    let x = send () x in
    close x;
    wait y

main : ()
main = 
    let (x1,x2) = new @X () in
    let (y1,y2) = new @Y () in
    fork @() (\_:()1-> server x2 y1);
    client x1 y2