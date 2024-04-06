{-

Based on the 'Ami and Boe' example from
    'Towards Races in Linear Logic', Wen Kokke, J. Garret Morris, And Philip Waddler

-}

type CakeStore   : *S = *?CakeService
type CakeService : 1S = &{ Cake: Wait
                         , Disappointment: Wait 
                         }

runCakeStore : dualof CakeStore -> Bool -> ()
runCakeStore cakeStore gotCake = 
    let (c, s)    = new @CakeService () in
    let cakeStore = send c cakeStore in
    if gotCake
    then 
        s |> select Cake |> close;
        runCakeStore cakeStore False
    else 
        s |> select Disappointment |> close;
        runCakeStore cakeStore False

ami : CakeStore -> ()
ami cakeStore = 
    let cakeService = fst @CakeService @CakeStore $ receive cakeStore in
    match cakeService with {
        Cake           c -> wait c; putStrLn "Ami got cake!",
        Disappointment c -> wait c; putStrLn "Ami got disappointment"
    }

boe : CakeStore -> ()
boe cakeStore =
    let cakeService = fst @CakeService @CakeStore $ receive cakeStore in
    match cakeService with {
        Cake           c -> wait c; putStrLn "Boe got cake!",
        Disappointment c -> wait c; putStrLn "Boe got disappointment"
    }

main : ()
main =
    let (c, s) = new @CakeStore () in
    fork (\_:() 1-> ami c);
    fork (\_:() 1-> boe c);
    runCakeStore s True
