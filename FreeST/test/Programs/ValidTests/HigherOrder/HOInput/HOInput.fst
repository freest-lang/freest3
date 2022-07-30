main : (Int, Skip)
main =
  let (w, r) = new !(?Int) in
  fork (
    let (ri, wi) = new ?Int in
    fork $ send ri w;
    send 5 wi);
  let (ri, _) = receive r in
  receive ri
-- Expect (5, Skip)  
