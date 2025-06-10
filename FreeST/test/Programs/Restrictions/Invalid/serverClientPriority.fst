type X = !a+2();!a+1();Close a

server : dualof X ->[top,a] ()
server x = 
    let (_,x) = receive x in
    let (_,x) = receive x in
    wait x

client : X ->[top,a] ()
client x = 
    let x = send () x in
    let x = send () x in
    close x

main : ()
main = 
    let (x1,x2) = new @X () in
    fork @() (\_:()1-> server x2);
    client x1