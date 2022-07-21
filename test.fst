
-- data Data = A | B
-- type Channel : 1S = &{C: Skip,D: Skip}

-- f : Data -> Channel -> Int
-- f A (C c) = 1
-- f A c     = 2
-- f x (C c) = 3
-- f x (D c) = 4

-- main : ()
-- main = 
--   let (w,r) = new Channel in
--   select C r;
--   printIntLn $ f A w
--   ;
--   let (w,r) = new Channel in
--   select D r;
--   printIntLn $ f B w


f : &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int
f (A (C c)) = 1
f c = 2

main : Int
main = 1