
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

data T = A Int | B Bool

f : T -> T -> Int 
f x y = -1
f (A x) (B x) = 0
f (A x) (B x) = 1

main : Int
main = 1