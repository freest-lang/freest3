{-

Based on the 'Ami and Boe' example from
    'Towards Races in Linear Logic', Wen Kokke, J. Garret Morris, And Philip Waddler

-}

type CakeStore   = *?CakeService
type CakeService = &{Cake: Close, Disappointment: Close}

runCakeStore : dualof CakeStore -> Bool -> Diverge
runCakeStore cakeStore gotCake = 
    let (c, s)    = new @CakeService () in
    let cakeStore = send c cakeStore in
    if gotCake
    then 
        s |> select Cake |> wait;
        runCakeStore cakeStore False
    else 
        s |> select Disappointment |> wait

storeClient : String -> CakeStore -> ()
storeClient name cakeStore =
    match receive_ @CakeService cakeStore with {
        Cake           c -> putStrLn (name ^^ " got cake!") ; close c,
        Disappointment c -> putStrLn (name ^^ " got disappointment") ; close c
    }

main : ()
main =
    let (c, s) = new @CakeStore () in
    fork (\_:() 1-> storeClient "Ami" c);
    fork (\_:() 1-> storeClient "Boe" c);
    runCakeStore s True
