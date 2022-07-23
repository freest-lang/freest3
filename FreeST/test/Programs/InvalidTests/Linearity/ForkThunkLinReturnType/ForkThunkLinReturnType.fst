-- The return type of the thunk received by fork should be
-- of an unrestricted kind, otherwise channels ends could be
-- left waiting indefinitely.
main : Int
main = let (r, w) = new ?Int in 
       fork @!Int (\_:() 1-> w);
       let (i, r) = receive r in i 
