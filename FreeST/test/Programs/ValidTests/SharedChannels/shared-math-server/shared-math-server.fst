type MathServer  = *?MathService
type MathService = +{ Plus   : !Int; !Int; ?Int; MathService
                         , Greater: !Int; !Int; ?Bool; MathService
                         , Neg    : !Int; ?Int; MathService
                         , End    : Close 
                         }

runMathService : dualof MathService -> ()
runMathService ch =
    match ch with {
        Plus    ch ->
            let (n1, ch) = receive ch in
            let (n2, ch) = receive ch in
            runMathService $ send (n1 + n2) ch,
        Greater ch ->
            let (n1, ch) = receive ch in
            let (n2, ch) = receive ch in
            runMathService $ send (n1 > n2) ch,
        Neg     ch -> 
            let (n1, ch) = receive ch in
            runMathService $ send (-n1) ch,
        End     ch -> wait ch
    }

runMathServer : dualof MathServer -> ()
runMathServer ch =
    let (c, s) = new @MathService () in
    let ch = send c ch in
    runMathService s;
    runMathServer ch

client1 : MathServer -> ()
client1 ch =
    let (c, _) = receive ch in
    let (n, c) = select Plus c 
               |> send 1
               |> send 2
               |> receive
               in
    let (m, c) = select Neg c
               |> send n
               |> receive
               in
    select End c |> close;
    print @Int m

client2 : MathServer -> ()
client2 ch =
    let (c, _) = receive ch in
    let (b, c) = select Greater c
               |> send 2
               |> send 1
               |> receive
               in
    select End c |> close; 
    print @Bool b

main : ()
main =
    let (c, s) = new @MathServer () in
    fork (\_:() 1-> client1 c);
    fork (\_:() 1-> client2 c);
    runMathServer s
