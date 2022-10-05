main : Int
main =
  let (w, r) = new !Int;End in
  let w1 = fork @() (\_:()1-> send 5 w |> close) in
-- Can't do this with synchronous channels because the writer blocks until it can synchronize with a reader.
--  let w1 = send w 5 in
  let (n, r1) = receive r in
  close r1;
  n
