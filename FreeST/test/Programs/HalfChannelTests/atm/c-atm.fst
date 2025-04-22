type AtmOps = &{ Deposit : ?Int; !Int ; AtmOps
              ,  Withdraw : ?Int ; +{Valid: Skip, Invalid: Skip}; AtmOps
              , Quit : Close
            }

type Result = +{Ok: AtmOps , Error: Wait}

type Atm = ?Int ; Result

atm' : Int -> AtmOps -> ()
atm' s (Deposit c) = 
    let (i, c1) = receive c in
    c1 |> send (s + i) |>  atm' (s + i)
atm' s (Withdraw c) =
    let (i, c1) = receive c in
    if s >= i
    then c1 |> select Valid |> atm' (s - i)
    else c1 |> select Invalid |> atm' s
atm' s (Quit c) = putStr "Goodbye...\n" ; close c 

atm : Atm -> () 
atm c =
    let (i, c1) = receive c in
    if i == 5 
    then c1 |> select Error |> wait
    else c1 |> select Ok |> atm' 100

atmClient' : dualof Result -> ()
atmClient' (Ok c) = 
    let (s, c1) = c |> select Deposit |> send 50 |> receive in
    putStr "Your balance is " ; putStr (show @Int s) ; putStr "\n" ;
    let c2 = c1 |> select Withdraw |> send 200 in
    atmClient'' @AtmOps c2 |> select Quit |> wait

atmClient' (Error c) = putStr "Client Error\n"; close c

atmClient'' : &{Valid: Skip, Invalid: Skip} ; a -> a 
atmClient'' (Valid c) =
    putStr "Withdrawal successful\n" ; c
atmClient'' (Invalid c) =
    putStr "Insufficient balance\n" ; c

atmClient : dualof Atm -> ()
atmClient c =
    let c1 = c |> send 4 in
    atmClient' c1

main : ()
main =
    forkWith @Atm @() atmClient
    |>
    atm