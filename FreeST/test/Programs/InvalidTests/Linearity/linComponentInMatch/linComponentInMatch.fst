type Server : 1S = &{A: !Int}

server : Server;Close 1-> ()
server s =
  match s with { A s -> () } -- here

main : Int
main = let (s, c) = new @(Server;Close) () in
       fork (\_:() 1-> server s);
       let (n, c) = select A c |> receive in wait c ; n
