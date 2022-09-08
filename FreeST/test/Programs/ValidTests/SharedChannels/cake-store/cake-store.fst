{-

Based on the 'Ami and Boe' example from
    'Towards Races in Linear Logic', Wen Kokke, J. Garret Morris, And Philip Waddler

-}

type CakeStore   : *S = *?CakeService
type CakeService : 1S = &{ Cake: End
                         , Disappointment: End 
                         }

runCakeStore : dualof CakeStore -> Bool -> ()
runCakeStore cakeStore gotCake = 
    let (c, s)    = new CakeService in
    let cakeStore = send c cakeStore in
    if gotCake
    then 
        select Cake s & close;
        runCakeStore cakeStore False
    else 
        select Disappointment s & close;
        runCakeStore cakeStore False

ami : CakeStore -> ()
ami cakeStore = 
    let cakeService = fst @CakeService @CakeStore $ receive cakeStore in
    match cakeService with {
        Cake           c -> close c; printStringLn "Ami got cake!",
        Disappointment c -> close c; printStringLn "Ami got disappointment"
    }

boe : CakeStore -> ()
boe cakeStore =
    let cakeService = fst @CakeService @CakeStore $ receive cakeStore in
    match cakeService with {
        Cake           c -> close c; printStringLn "Boe got cake!",
        Disappointment c -> close c; printStringLn "Boe got disappointment"
    }

main : ()
main =
    let (c, s) = new CakeStore in
    fork (\_:() 1-> ami c);
    fork (\_:() 1-> boe c);
    runCakeStore s True