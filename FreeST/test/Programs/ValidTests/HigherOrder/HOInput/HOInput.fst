main : Int
main =
  let (w, r) = new @(!(?Int;Wait);Close) () in
  fork @() (\_:()1-> 
    let (ri, wi) = new @(?Int;Wait) () in
    fork @() (\_:()1-> send ri w |> close);
    send 5 wi |> close);
    r |> receiveAndWait @(?Int;Wait) 
      |> receiveAndWait @Int
-- Expect 5
