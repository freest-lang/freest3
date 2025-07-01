type CheckIn = !a();Close b
type MathService = +c{Negate: !d1 Int;?d2 Int
                    , IsZero: !e1 Int;?e2 Bool
                    };Close f

mathClient : CheckIn ->[top,bot] MathService 1->[a,f] Int
mathClient x m =
    x |> send () |> close;
    let m = select Negate m in
    let m = send 5 m in
    let (n, m) = receive m in
    close m;
    n

mathServer : dualof CheckIn ->[top,bot] dualof MathService 1->[a,f] ()
mathServer x (Negate m) =
    let (_,x) = receive x in
    wait x;
    let (n, m) = receive m in
    let m = send (-n) m in
    wait m
mathServer x (IsZero m) =
    let (_,x) = receive x in
    wait x;
    let (n, m) = receive m in
    let m = send (n == 0) m in
    wait m

main : Int
main =
    let (x1, x2) = new @CheckIn () in
    let (m1, m2) = new @MathService () in
    fork (\_:()1-> mathServer x2 m2);
    mathClient x1 m1