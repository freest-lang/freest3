type Conversation1 = +a1{ SendFirst: !a2_1(); ?a3_1(), ReceiveFirst: ?a2_2(); !a3_2()};Close a4
type Conversation2 = +b1{ SendFirst: !b2_1(); ?b3_1(), ReceiveFirst: ?b2_2(); !b3_2()};Close b4

personA : Conversation1 ->[top,bot] Conversation2 1->[a1,b4] ()
personA c1 c2 =
    let c1 = select SendFirst c1 in
    let c2 = select SendFirst c2 in
    let c1 = send () c1 in
    let c2 = send () c2 in
    let (_, c1) = receive c1 in
    let (_, c2) = receive c2 in
    close c1;
    close c2

personB : dualof Conversation1 ->[top,bot] dualof Conversation2 1->[a1,bot] ()
personB (SendFirst c1) (ReceiveFirst c2) =
    let (_,c1) = receive c1 in
    let c1 = send () c1 in
    wait c1;
    let c2 = send () c2 in
    let (_,c2) = receive c2 in
    wait c2
personB (ReceiveFirst c1) (SendFirst c2) =
    let c1 = send () c1 in
    let (_,c1) = receive c1 in
    wait c1;
    let (_,c2) = receive c2 in
    let c2 = send () c2 in
    wait c2


main : ()
main =
    let (c1, c2) = new @Conversation1 () in
    let (c3, c4) = new @Conversation2 () in
    fork @() (\_:()1-> personA c1 c3);
    personB c2 c4