
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

data T = A | A

type Channel : 1S = &{A:Skip, B: Skip, C: Skip}

type Channel2 : 1S = &{A:Skip, D: Skip, E: Skip}

-- f : &{A:Skip, C: Skip} 1-> Int 
-- f (A c) = 0
-- f (C c) = 1

-- g : &{A:Skip, B: Skip} 1-> Int 
-- g (A c) = 0
-- g (B c) = 1

main : Int
main = 1