
-- sink : Skip -> ()
-- sink c = ()

-- data Data = A | B
-- type Channel : 1S = &{C: Skip,D: Skip}

-- f : Data -> Channel -> Int
-- f A (C c) = 1
-- f B (D c) = 2

-- main : ()
-- main = 
--   let (w,r) = new Channel in
--   select C r;
--   printIntLn $ f A w
--   ;
--   let (w,r) = new Channel in
--   select D r;
--   printIntLn $ f B w


data T = A | B

f : T -> T -> Int
f A A = 0
f A = 1

main : Int
main = f A A