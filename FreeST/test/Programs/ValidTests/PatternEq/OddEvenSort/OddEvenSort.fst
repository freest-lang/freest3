type Sorter : SL = +{Done: Skip, More: !Int ; ?Int; Sorter}

-- first accepts the number of phases, the value in the node, the
-- channel to the right and the channel where to announce the result
-- once done. First is an odd process, hence it controls when sorting
-- is completed.
first : Int -> Int -> Sorter -> !Int -o Skip
first 0 x right collect' = let _ = select Done right in send x collect'
first n x right collect' =
  let (min, right) = exchangeRight x right in
  first (n - 1) min right collect'

-- evenProcess accepts the number of phases, the value in the node,
-- the channel to the left, the channel to the right and the channel
-- where to announce the result once complete. evenProcess receives
-- from the left the announcement that sorting is completed (Done).
evenProcess : Int -> Int -> dualof Sorter -> Sorter -o !Int -o Skip
evenProcess n x left right collect' =
  match left with {
    Done left -> let _ = select Done right in send x collect',
    More left -> let (max, left) = exchangeLeft x left in
                 oddProcess (n - 1) max left right collect'
  }

-- oddProcess accepts the number of phases, the value in the node, the
-- channel to the left, the channel to the right and the channel where
-- to announce the result once done. oddProcess is an odd process,
-- hence it controls when sorting is complete.
oddProcess : Int -> Int -> dualof Sorter -> Sorter -o !Int -o Skip
oddProcess 0 x left right collect' =
  let _ = select Done right in consume left ; send x collect'
oddProcess n x left right collect' =
  let (min, right) = exchangeRight x right in
  evenProcess (n - 1) min left right collect'

-- last accepts the value in the node, the channel to the left and the
-- channel where to announce the result once done. last receives from
-- the left the announcement that sorting is completed (Done).
last : Int -> dualof Sorter -> !Int -o Skip
last x left collect' =
  match left with {
    Done left -> send x collect',
    More left -> let (max, left) = exchangeLeft x left in
                 last max left collect'
  }

-- Exchange a value with a right node; return the min and the channel.
exchangeRight : Int -> Sorter -> (Int, Sorter)
exchangeRight x right =
  let (y, right) = receive (send x (select More right)) in
  (min x y, right)

-- Exchange a value with a right node; return the max and the channel.
exchangeLeft : Int -> ?Int;!Int;dualof Sorter -> (Int, dualof Sorter)
exchangeLeft x left =
  let (y, left) = receive left in
  (max x y, send x left)

-- Consume the rest of a left channel once sorting in complete for an
-- odd process. The More branch is never exercised.
consume : dualof Sorter -> ()
consume c =
  match c with {
    Done c -> (),
    More c -> -- Should not happen
      let (_, c) = receive c in
      consume (send (-99) c)
  }

main : ()
main =
  -- number of inner processes
  let p = 5 in
  -- left-right communication channels
  let (l1, r1) = new Sorter in
  let (l2, r2) = new Sorter in
  let (l3, r3) = new Sorter in
  let (l4, r4) = new Sorter in
  let (l5, r5) = new Sorter in
  let (l6, r6) = new Sorter in
  -- collect' channels
  let (cw1, cr1) = new !Int in
  let (cw2, cr2) = new !Int in
  let (cw3, cr3) = new !Int in
  let (cw4, cr4) = new !Int in
  let (cw5, cr5) = new !Int in
  let (cw6, cr6) = new !Int in
  let (cw7, cr7) = new !Int in
  -- the various sorting nodes
  fork[Skip] (first       (p / 2)     99    l1 cw1);
  fork[Skip] (evenProcess (p / 2)     88 r1 l2 cw2);
  fork[Skip] (oddProcess  (p / 2 - 1) 33 r2 l3 cw3);
  fork[Skip] (evenProcess (p / 2)     11 r3 l4 cw4);
  fork[Skip] (oddProcess  (p / 2 - 1) 55 r4 l5 cw5);
  fork[Skip] (evenProcess (p / 2)     44 r5 l6 cw6);
  fork[Skip] (last                    77 r6    cw7);
  -- collect' and print results
  let (x1, _) = receive cr1 in printIntLn x1;
  let (x2, _) = receive cr2 in printIntLn x2;
  let (x3, _) = receive cr3 in printIntLn x3;
  let (x4, _) = receive cr4 in printIntLn x4;
  let (x5, _) = receive cr5 in printIntLn x5;
  let (x6, _) = receive cr6 in printIntLn x6;
  let (x7, _) = receive cr7 in printIntLn x7


-- Auxiliary function because of fork : () -> ()
sink : Skip -> ()
sink _ = ()
