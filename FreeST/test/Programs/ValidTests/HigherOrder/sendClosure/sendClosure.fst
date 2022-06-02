-- | Create a new child process and a linear channel through which it can 
--   communicate with its parent process. Return the channel endpoint.
forkWith : ∀ a:SL b:TU . (dualof a -> b) -> a
forkWith f =
  let (x, y) = new a in
  fork $ f y;
  x

plus : Int -> Int -> Int
plus x y = x + y

main : Int
main =
  let n = 23 in
  let x = forkWith [?(Int -> Int)] [Skip] (\y: !(Int -> Int) -> send (plus n) y) in
  (fst [Int -> Int, Skip] (receive x)) 27
