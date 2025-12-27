type Conversation = +a{ SendFirst: !b(); ?c(), ReceiveFirst: ?d(); !e()};Close f

personA : Conversation 1->[top,f] ()
personA c =
    let c = select SendFirst c in
    let c = send () c in
    let (_, c) = receive c in
    close c

personB : dualof Conversation 1->[top,f] ()
personB (SendFirst c) =
    let (_,c) = receive c in
    let c = send () c in
    wait c
personB (ReceiveFirst c) =
    let c = send () c in
    let (_,c) = receive c in
    wait c

personC : Conversation 1->[top,f] ()
personC c =
    let c = select ReceiveFirst c in
    let (_, c) = receive c in
    let c = send () c in
    close c

personD : dualof Conversation 1->[top,f] ()
personD (SendFirst c) =
    let (_,c) = receive c in
    let c = send () c in
    wait c
personD (ReceiveFirst c) =
    let c = send () c in
    let (_,c) = receive c in
    wait c

main : ()
main =
    let (c1, c2) = new @Conversation () in
    let (c3, c4) = new @Conversation () in
    fork (\_:()1-> personA c1);
    fork (\_:()1-> personC c3);
    fork (\_:()1-> personB c2);
    personD c4