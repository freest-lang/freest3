type X = !a();!a+1();Close a+2

server : dualof X ->[top,a+2] ()
server x = 
    let (_,x) = receive x in
    let (_,x) = receive x in
    wait x

client : X ->[top,a+2] ()
client x = 
    let x = send () x in
    let x = send () x in
    close x

main : ()
main = 
    let (x1,x2) = new @X () in
    fork @() (\_:()1-> server x2);
    client x1