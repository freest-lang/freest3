type X = !p();Close
type Y = !q();Close

server : dualof X ->[Bottom,Bottom] Y 1->[Bottom,Bottom] ()
server x y = 
    let (_,x) = receive x in
    let y = send () y in
    wait x;
    close y

client : X ->[Bottom,Bottom] dualof Y 1->[Bottom,Bottom] ()
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