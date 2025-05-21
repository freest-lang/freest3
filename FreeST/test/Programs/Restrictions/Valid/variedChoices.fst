type Conversation = +1{ SendFirst: !2(); ?3(), ReceiveFirst: ?2(); !3()};Close 8
type Test = +10{ One: +11{Three: Close 13}, Two: +12{Four: Close 14}}

personA : Conversation 1->[1,4] ()
personA c =
    let c = select SendFirst c in
    let c = send () c in
    let (_, c) = receive c in
    close c

personB : dualof Conversation 1->[1,4] ()
personB (SendFirst c) =
    let (_,c) = receive c in
    let c = send () c in
    wait c
personB (ReceiveFirst c) =
    let c = send () c in
    let (_,c) = receive c in
    wait c

personC : Conversation 1->[1,4] ()
personC c =
    let c = select ReceiveFirst c in
    let (_, c) = receive c in
    let c = send () c in
    close c

personD : dualof Conversation 1->[1,4] ()
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
    fork @() (\_:()1-> personA c1);
    fork @() (\_:()1-> personC c3);
    fork @() (\_:()1-> personB c2);
    personD c4