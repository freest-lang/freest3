{-

Based on the 'Ami and Boe' example from
    'Towards Races in Linear Logic', Wen Kokke, J. Garret Morris, And Philip Waddler

-}

type CakeStore   = *?CakeService
type CakeService = &{Cake: Wait, Disappointment: Wait}

runCakeStore : dualof CakeStore -> Bool -> Diverge
runCakeStore cakeStore gotCake = 
    let (c, s)    = new @CakeService () in
    let cakeStore = send c cakeStore in
    if gotCake
    then 
        s |> select Cake |> close;
        runCakeStore cakeStore False
    else 
        s |> select Disappointment |> close

storeClient : String -> CakeStore -> ()
storeClient name cakeStore =
    match receive_ @CakeService cakeStore with {
        Cake           c -> putStrLn (name ^^ " got cake!") ; wait c,
        Disappointment c -> putStrLn (name ^^ " got disappointment") ; wait c
    }

sleep : Int -> ()
sleep n = if n == 0 then () else sleep (n - 1)

main : ()
main =
    let (c, s) = new @CakeStore () in
    fork (\_:() 1-> storeClient "Ami" c);
    fork (\_:() 1-> storeClient "Boe" c);
    runCakeStore s True;
	sleep 10000
