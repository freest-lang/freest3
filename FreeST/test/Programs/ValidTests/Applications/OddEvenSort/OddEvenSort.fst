type Sorter = +{Done: Close, More: !Int ; ?Int; Sorter}

-- Exchange a value with a right node; return the min and the channel.
exchangeRight : Int -> Sorter -> (Int, Sorter)
exchangeRight x right =
  let (y, right) = receive (send x (select More right)) in
  (min x y, right)

-- first accepts the number of phases, the value in the node, the
-- channel to the right and the channel where to announce the result
-- once done. First is an odd process, hence it controls when sorting
-- is completed.
first : Int -> Int -> Sorter -> !Int;Close 1-> ()
first n x right collect' =
  if n == 0
  then select Done right |> close ; 
       send x collect' |> close
  else let (min, right) = exchangeRight x right in
       first (n - 1) min right collect'

-- Exchange a value with a right node; return the max and the channel.
exchangeLeft : Int -> ?Int;!Int;dualof Sorter -> (Int, dualof Sorter)
exchangeLeft x left =
  let (y, left) = receive left in
  (max x y, send x left)

-- Consume the rest of a left channel once sorting in complete for an
-- odd process. The More branch is never exercised.
consume' : dualof Sorter -> ()
consume' c =
  match c with {
    Done c -> wait c,
    More c -> -- Should not happen
      let (_, c) = receive c in
      consume' (send (-99) c)
  }

-- oddProcess accepts the number of phases, the value in the node, the
-- channel to the left, the channel to the right and the channel where
-- to announce the result once done. oddProcess is an odd process,
-- hence it controls when sorting is complete.

-- evenProcess accepts the number of phases, the value in the node,
-- the channel to the left, the channel to the right and the channel
-- where to announce the result once complete. evenProcess receives
-- from the left the announcement that sorting is completed (Done).

oddProcess and evenProcess : Int -> Int -> dualof Sorter -> Sorter 1-> !Int;Close 1-> ()
oddProcess n x left right collect' =
  if n == 0
  then select Done right |> close ; consume' left ; send x collect' |> close
  else let (min, right) = exchangeRight x right in
       evenProcess (n - 1) min left right collect'

evenProcess n x left right collect' =
  match left with {
    Done left -> wait left; select Done right |> close ; send x collect' |> close,
    More left -> let (max, left) = exchangeLeft x left in
                 oddProcess (n - 1) max left right collect'
  }

-- last accepts the value in the node, the channel to the left and the
-- channel where to announce the result once done. last receives from
-- the left the announcement that sorting is completed (Done).
last : Int -> dualof Sorter -> !Int;Close 1-> ()
last x left collect' =
  match left with {
    Done left -> wait left; send x collect' |> close,
    More left -> let (max, left) = exchangeLeft x left in
                 last max left collect'
  }

main : ()
main =
  -- number of inner processes
  let p = 5 in
  -- left-right communication channels
  let (l1, r1) = new @Sorter () in
  let (l2, r2) = new @Sorter () in
  let (l3, r3) = new @Sorter () in
  let (l4, r4) = new @Sorter () in
  let (l5, r5) = new @Sorter () in
  let (l6, r6) = new @Sorter () in
  -- collect' channels
  let (cw1, cr1) = new @(!Int;Close) () in
  let (cw2, cr2) = new @(!Int;Close) () in
  let (cw3, cr3) = new @(!Int;Close) () in
  let (cw4, cr4) = new @(!Int;Close) () in
  let (cw5, cr5) = new @(!Int;Close) () in
  let (cw6, cr6) = new @(!Int;Close) () in
  let (cw7, cr7) = new @(!Int;Close) () in
  -- the various sorting nodes
  fork (\_:() 1-> first       (p / 2)     99    l1 cw1);
  fork (\_:() 1-> evenProcess (p / 2)     88 r1 l2 cw2);
  fork (\_:() 1-> oddProcess  (p / 2 - 1) 33 r2 l3 cw3);
  fork (\_:() 1-> evenProcess (p / 2)     11 r3 l4 cw4);
  fork (\_:() 1-> oddProcess  (p / 2 - 1) 55 r4 l5 cw5);
  fork (\_:() 1-> evenProcess (p / 2)     44 r5 l6 cw6);
  fork (\_:() 1-> last                    77 r6    cw7);
  -- collect' and print results
  let x1 = receiveAndWait @Int cr1 in print @Int x1;
  let x2 = receiveAndWait @Int cr2 in print @Int x2;
  let x3 = receiveAndWait @Int cr3 in print @Int x3;
  let x4 = receiveAndWait @Int cr4 in print @Int x4;
  let x5 = receiveAndWait @Int cr5 in print @Int x5;
  let x6 = receiveAndWait @Int cr6 in print @Int x6;
  let x7 = receiveAndWait @Int cr7 in print @Int x7

