type X = !p+2();!p+1();Close p

server : dualof X ->[top,p] ()
server x = 
    let (_,x) = receive x in
    let (_,x) = receive x in
    wait x

client : X ->[top,p] ()
client x = 
    let x = send () x in
    let x = send () x in
    close x

main : ()
main = 
    let (x1,x2) = new @X () in
    fork (\_:()1-> server x2);
    client x1