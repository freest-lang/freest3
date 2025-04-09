type X = !2();!1();Close 3

server : dualof X ->[3,1] ()
server x = 
    let (_,x) = receive x in
    let (_,x) = receive x in
    wait x

client : X ->[3,1] ()
client x = 
    let x = send () x in
    let x = send () x in
    close x

main : ()
main = 
    let (x1,x2) = new @X () in
    fork @() (\_:()1-> server x2);
    client x1