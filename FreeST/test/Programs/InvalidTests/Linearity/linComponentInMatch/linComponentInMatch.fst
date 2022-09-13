type Server : 1S = &{A: !Int}

server : Server 1-> ()
server s =
  match s with { A s -> () } -- here

main : Int
main = let (s, c) = new Server in
       fork (server s);
       let (n, _) = select A c |> receive in n
