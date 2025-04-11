type CheckIn = !2() ; Close 3

type MathService = +1{ Negate: !4Int ; ?5Int
                    , IsZero: !6Int ; ?7Bool
                    } ; Close 8

mathClient : CheckIn ->[top,bot] MathService 1->[1,8] Int
mathClient x c0 =
    let c1 = select Negate c0 in
    let c2 = send 5 c1 in
    let (i, c3) = receive c2 in
    close c3;
    x |> send () |> close;
    i

-- mathClient : MathService ->[1,5] Int
-- mathClient c =
--   c |> select Negate |> send 5 |> receiveAndClose @Int

mathServer : dualof CheckIn ->[top,bot] dualof MathService 1->[1,8] ()
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

-- mathServer : dualof MathService ->[1,5] ()
-- mathServer (Negate c1) =
--       let (i, c2) = receive c1 in
--       sendAndWait @Int (-i) c2
-- mathServer (IsZero c1) =
--       let (i, c2) = receive c1 in
--       sendAndWait @Bool (i == 0) c2

main : Int
main =
    let (x1, x2) = new @CheckIn () in
    let (c1, c2) = new @MathService () in
    fork @() (\_:()1-> mathServer x2 c2);
    mathClient x1 c1