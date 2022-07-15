
data Data = A | B
type Channel : 1S = &{C: Skip,D: Skip}

f : Channel -> Data 1-> Int
f &(C c) A = 1
f &(D c) B = 2
f c y = 3


main : ()
main = 
  let (w,r) = new Channel in
  select C r;
  printIntLn $ f w A 
  ;
  let (w,r) = new Channel in
  select D r;
  printIntLn $ f w B

-- f : Channel -> Int
-- f &(C c) = 1
-- -- f &(D c) = 2
-- f c = 2

-- main : ()
-- main = 
--   let (w,r) = new Channel in
--   select C r;
--   printIntLn $ f w