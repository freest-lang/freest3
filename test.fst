
sink : Skip -> ()
sink c = ()

data Data = A | B
type Channel : 1S = &{C: Skip,D: Skip}

f : Data -> Channel -> Int
f A (C c) = 1 
f B c = 
  match c with {
    D c -> 2
  }

g : Channel -> Data -> Int
g c x = 
  match c with {
    C c -> 
      case x of {
        A -> sink c; 1,
        B -> sink c; 2
      },
    D c -> 
      case x of {
        A -> sink c; 1,
        B -> sink c; 2
      }
  }

main : Int 
main =
  let (w,r) = new Channel in
  select C r;
  g w A
  -- f A w

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