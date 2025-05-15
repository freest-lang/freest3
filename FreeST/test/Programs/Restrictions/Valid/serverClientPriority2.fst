type X = !3();?1();Close 4

server : dualof X ->[3,4] ()
server x = 
    let (_,x) = receive x in
    let x = send () x in
    wait x

client : X ->[3,4] ()
client x = 
    let x = send () x in
    let (_,x) = receive x in
    close x

main : ()
main = 
    let (x1,x2) = new @X () in
    fork @() (\_:()1-> server x2);
    client x1