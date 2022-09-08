main : Int
main =
  let (w, r) = new !(?Int;End);End in
  fork @() (\_:()1-> 
    let (ri, wi) = new ?Int;End in
    fork @() (\_:()1-> send ri w & close);
    send 5 wi & close);
  let (ri, r) = receive r in
  close r;
  let (n, ri) = receive ri in
  close ri;
  n
-- Expect 5
