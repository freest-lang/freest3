{-

Based on the 'Ami and Boe' example from
    'Towards Races in Linear Logic', Wen Kokke, J. Garret Morris, And Philip Waddler

-}

type CakeStore   = *?CakeService
type CakeService = &{Cake: Close, Disappointment: Close}

type Fork = *+{Over}
type Join = dualof Fork

waitFor : Int -> Join -> ()
waitFor n join =
	if n == 0
	then ()
	else match join with { Over _ -> waitFor (n - 1) join }

handleClient : CakeService -> Bool -> Fork -> ()
handleClient s gotCake f =
	if gotCake
	then
		s |> select Cake |> wait; 
		select Over f; ()
	else
		s |> select Disappointment |> wait; 
		select Over f; ()

runCakeStore : Bool -> Int -> Int -> dualof CakeStore -> (Fork, Join) -> ()
runCakeStore gotCake k n cakeStore fj =
	if k == 0 then waitFor n (snd @Fork @Join fj)
	else
		let s = accept @CakeService cakeStore in
		fork (\_:() 1-> handleClient s gotCake (fst @Fork @Join fj));
		runCakeStore False (k - 1) n cakeStore fj
	

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
	fork (\_:() 1-> storeClient "Cai" c);
    runCakeStore True 3 3 s (new @Fork ())
