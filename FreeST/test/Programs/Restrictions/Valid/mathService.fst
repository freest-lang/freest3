type CheckIn = !2() ; Close 3

type MathService = +1{ Negate: !4Int ; ?5Int
                    , IsZero: !6Int ; ?7Bool
                    } ; Close 8

mathClient : CheckIn ->[1,bot] MathService 1->[1,8] Int
mathClient x c0 =
    let c1 = select Negate c0 in
    x |> send () |> close;
    let c2 = send 5 c1 in
    let (i, c3) = receive c2 in
    close c3;
    i

mathServer : dualof CheckIn ->[1,bot] dualof MathService 1->[1,bot] ()
mathServer x (Negate c1) =
    let (_,x) = receive x in
    wait x;
    let (i, c2) = receive c1 in
    c2 |> send (-i) |> wait
mathServer x (IsZero c1) =
    let (_,x) = receive x in
    wait x;
    let (i, c2) = receive c1 in
    c2 |> send (i == 0) |> wait

main : Int
main =
    let (x1, x2) = new @CheckIn () in
    let (c1, c2) = new @MathService () in
    fork @() (\_:()1-> mathServer x2 c2);
    mathClient x1 c1