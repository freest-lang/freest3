type Server = &{A: !Int};Wait

server : Server -> () 1-> ()
server s _ =
  match s with { A s -> () } -- here

main : Int
main = let (s, c) = new @Server () in
       fork (server s);
       c |> select A |> receiveAndClose @Int
