main : Int
main = let (r,w) = new @?Int () in 
       fork @!Int (\_:() 1-> w);
       let (i, _) = receive r in
       i 
