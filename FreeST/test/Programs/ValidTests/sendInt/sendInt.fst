main : Int
main =
  let (w, r) = new !Int;Skip in
  let w1 = fork (send w 5) in
-- Can't do this with synchronous channels because the writer blocks until it can synchronize with a reader.
--  let w1 = send w 5 in
  let (n, r1) = receive r in
  n
