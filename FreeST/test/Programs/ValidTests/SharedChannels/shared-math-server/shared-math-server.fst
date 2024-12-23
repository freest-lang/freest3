type MathServer  = *?MathService
type MathService = +{ Plus: !Int; !Int; ?Int; MathService
                    , Gt  : !Int; !Int; ?Bool; MathService
                    , Neg : !Int; ?Int; MathService
                    , Done: Close 
                    }

runMathService : dualof MathService -> ()
runMathService (Plus s) =
  let (n1, s) = receive s in
  let (n2, s) = receive s in
  runMathService $ send (n1 + n2) s
runMathService (Gt s) =
  let (n1, s) = receive s in
  let (n2, s) = receive s in
  runMathService $ send (n1 > n2) s
runMathService (Neg s) =
  let (n1, s) = receive s in
  runMathService $ send (-n1) s
runMathService (Done s) =
  wait s

-- Serve one client at a time. Try variant where runMathService runs on a
-- separate thread, for better throughput.
runMathServer : dualof MathServer -> ()
runMathServer ch =
  runMathService (accept @MathService c);
  runMathServer ch

client1 : MathServer -> ()
client1 ch =
  let c = receive_ @MathService ch in
  let (n, c) = c |> select Plus |> send 1 |> send 2 |> receive in
  let (m, c) = c |> select Neg |> send n |> receive in
  c |> select Done |> close;
  print @Int m

client2 : MathServer -> ()
client2 ch =
  let c = receive_ @MathService ch in
  let (b, c) = c |> select Gt |> send 2 |> send 1 |> receive in
  c|> select Done |> close; 
  print @Bool b

main : ()
main =
  let (c, s) = new @MathServer () in
  fork (\_:() 1-> client1 c);
  fork (\_:() 1-> client2 c);
  runMathServer s
