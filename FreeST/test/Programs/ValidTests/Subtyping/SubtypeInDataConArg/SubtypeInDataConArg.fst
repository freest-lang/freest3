data List : 1T = Nil () | Cons (Int 1-> Int) List 

type SendList : 1S = +{More: !(Int 1-> Int);SendList, Stop: Skip}

sendList : forall a:1S . List -> SendList;a 1-> a 
sendList (Nil _    ) c = c |> select Stop 
sendList (Cons f fs) c = sendList @a fs (c |> select More |> send f)

recvList : forall a:1S . dualof SendList;a -> (List, a) 
recvList (Stop c) = (Nil(), c)
recvList (More c) = let (f , c) = receive c  in 
                    let (fs, c) = recvList @a c in 
                    (Cons f fs, c)

applyAll : List -> Int 1-> Int
applyAll (Nil _    ) n = n
applyAll (Cons f fs) n = applyAll fs (f n) 

main : Int
main = let (o,i) = new @(SendList;Close) () in
       let fs = Cons succ $ Cons pred (Nil()) in     -- here we provide a list of linear functions
       fork (\_:() 1-> sendList @Close fs o |> close); -- where a list of unrestricted ones is required
       let (fs,i) = recvList @Wait i in 
       wait i;
       applyAll fs 0