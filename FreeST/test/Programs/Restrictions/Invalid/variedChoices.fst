type ConversationA = +a1{ SendFirst: !a2(); ?a3(), ReceiveFirst: ?a4(); !a5()};Close a6
type ConversationB = +b1{ SendFirst: !b2(); ?b3(), ReceiveFirst: ?b4(); !b5()};Close b6

personA : ConversationA ->[top,bot] ConversationB 1->[a1,b6] ()
personA c1 c2 =
    let c1 = select SendFirst c1 in
    let c2 = select SendFirst c2 in
    let c1 = send () c1 in
    let c2 = send () c2 in
    let (_, c1) = receive c1 in
    let (_, c2) = receive c2 in
    close c1;
    close c2

personB : dualof ConversationA ->[top,bot] dualof ConversationB 1->[a1,b6] ()
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
    let (c1, c2) = new @ConversationA () in
    let (c3, c4) = new @ConversationB () in
    fork (\_:()1-> personA c1 c3);
    personB c2 c4