main : Int
main =
  let (w, r) = new @(!(?Int;EndW);EndC) () in
  fork @() (\_:()1-> 
    let (ri, wi) = new @(?Int;EndW) () in
    fork @() (\_:()1-> send ri w |> close);
    send 5 wi |> close);
    r |> receiveAndWait @(?Int;EndW) 
      |> receiveAndWait @Int
-- Expect 5
