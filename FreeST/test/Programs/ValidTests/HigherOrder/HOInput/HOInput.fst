main : Int
main =
  let (w, r) = new @(!(?Int;End);End) () in
  fork @() (\_:()1-> 
    let (ri, wi) = new @(?Int;End) () in
    fork @() (\_:()1-> send ri w |> close);
    send 5 wi |> close);
    r |> receiveAndClose @(?Int;End) 
      |> receiveAndClose @Int
-- Expect 5
