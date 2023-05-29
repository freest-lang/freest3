main : Int
main =
  let (w, r) = new @(!Int;EndC) () in
  fork @() (\_:()1-> send 5 w |> close); 
-- Can't do this with synchronous channels because the writer blocks until it can synchronize with a reader.
--  let w1 = send w 5 in
  receiveAndWait @Int r 
