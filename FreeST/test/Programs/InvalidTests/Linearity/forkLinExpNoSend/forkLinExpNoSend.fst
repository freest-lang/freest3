main : Int
main = let (r,w) = new @(?Int;Wait) () in 
       fork @(!Int;Close) (\_:() 1-> w);
       let (i, _) = receive r in
       i 
