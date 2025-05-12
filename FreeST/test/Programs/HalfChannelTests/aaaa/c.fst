-- Production S0
type Stack  = +{  Push: !Int ; Stack, 
      	          Pop  : ?Int, 
	                Peek: ?Int ; Stack} ; Close

client : Stack -> ()
client c = 
  let (_, c1) = c |> select Push |> send 1 |> select Push |> send 1 |> select Pop |> receive in
  let c2 = select  in

main : ()
main =
  newHcClient @(S0;Close) (("127.0.0.1", "8080"), "127.0.0.1:8081") |>
  client ;
  ()