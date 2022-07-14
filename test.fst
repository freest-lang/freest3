
sink : Skip -> ()
sink c = ()

data Data    = A | B
type Channel = &{C: Skip
                ,D: Skip
                }

f : Data -> Channel -> Int
-- f A (C c) = 1
f A c = 
  match c with {
    C c -> 1
    -- D c -> 2
  }

main : Int 
main =
  let (w,r) = new Channel in
  select D r;
  f A w

-- f : T -> C -> Int
-- f A (C c) = 1
-- f B (D c) = 2

-- g : C -> T -> Int
-- g (C c) A = 3
-- g (D c) B = 4

-- main : Int
-- main = 
--   let (w,r) = new C  in
--   select C r;
--   let i =     f A w  in
--   let (w,r) = new C  in
--   select D r;
--   let i = i + f B w  in
--   let (w,r) = new C  in
--   select C r;
--   let i = i + g w A  in
--   let (w,r) = new C  in
--   select D r;
--   let i = i + g w B  in
--   i  