{-

Based on the 'Ami and Boe' example from
    'Towards Races in Linear Logic', Wen Kokke, J. Garret Morris, And Philip Waddler

-}

type CakeStore   : SU = *?CakeService
type CakeService : SL = &{ Cake: Skip
                         , Disappointment: Skip
                         }

runCakeStore : dualof CakeStore -> Bool -> ()
runCakeStore cakeStore gotCake = 
    let (c, s)    = new CakeService in
    let cakeStore = send c cakeStore in
    if gotCake
    then 
        let _ = select Cake s in 
        runCakeStore cakeStore False
    else 
        let _ = select Disappointment s in 
        runCakeStore cakeStore False

ami : CakeStore -> ()
ami cakeStore = 
    let cakeService = fst[CakeService, CakeStore] $ receive cakeStore in
    match cakeService with {
        Cake           _ -> printStringLn "Ami got cake!",
        Disappointment _ -> printStringLn "Ami got disappointment"
    }

boe : CakeStore -> ()
boe cakeStore =
    let cakeService = fst[CakeService, CakeStore] $ receive cakeStore in
    match cakeService with {
        Cake           _ -> printStringLn "Boe got cake!",
        Disappointment _ -> printStringLn "Boe got disappointment"
    }

main : ()
main =
    let (c, s) = new CakeStore in
    fork $ ami c;
    fork $ boe c;
    runCakeStore s True