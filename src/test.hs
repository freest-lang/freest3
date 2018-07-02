-- f1 :: Int -> ()
-- f1 w =
--   let x = fork (client w) in
--   x

-- client :: Int -> Int            
-- client x = x


-- start :: Bool
-- start = let w,r = (3,True) in r

-- start :: Bool
-- start =
--   let w,r = new +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} in
--   let x = fork (boolServer r) in
--   client1 w

-- client1 :: +{And: !Bool;!Bool;?Bool;Skip, Or: !Bool;!Bool;?Bool;Skip, Not: !Bool;?Bool;Skip} -> Bool
-- client1 w =
--   let w1 = select And w in
--   let w2 = send True w1 in
--   let r1 = send False w2 in
--   let x, r2 = receive r1 in
--   x

-- boolServer :: &{And: Skip;?Bool;?Bool;!Bool;Skip,
--                 Or: Skip;?Bool;?Bool;!Bool;Skip,
--                 Not: Skip;?Bool;!Bool} -> ()
-- boolServer c =
--   match c with
--     And c1 -> 
--       let n1, c2 = receive c1 in
--       let n2, c3 = receive c2 in
--       let x = send (n1 && n2) c3 in
--       ()

--     Or c1 -> 
--       let n1, c2 = receive c1 in
--       let n2, c3 = receive c2 in
--       let x = send (n1 || n2) c3 in 
--       ()

--     Not c1 -> 
--       let n1, c2 = receive c1 in
--       let x = send (not n1) c2 in
--       ()

-- data Tree = Leaf | Node Int Tree Tree

-- start :: Tree
-- start = Node 10 Leaf (Node 2 Leaf Leaf)

-- myId :: forall a => a -> a
-- myId x = x

-- t1 :: Int
-- t1 = myId 1

-- t2 :: Int
-- t2 = myId [Int] 1

-- simpleIf :: Int
-- simpleIf = if 2 then 2 else 2


-- erroneousIf :: Int 
-- erroneousIf =
--   let y =
--     if True then
--       let x = 5 in 7
--     else
--       let x = 3 in 9
--   in x

-- binletT :: Int
-- binletT =
--   let x, y = (1,2) in x

-- binletF :: Int
-- binletF = let x, y = 1 in x 
  
-- new1 :: (+{A:!Int},&{A:?Int})
-- new1 = new +{A:!Int};?Int

-- client :: !Int;?Bool;Skip -> ()
-- client c =
--   let c1 = send 5 c in
--   let b, c2 = receive c1
--   in ()

-- start :: Int
-- start =
--   let w,r = new +{Opp: !Int;?Int;Skip, Plus: !Int;!Int;?Int;Skip} in
--   let x = fork (mathServer r) in
--   let w1 = select Plus w in
--   let w2 = send 5 w1 in
--   let r1 = send 18 w2 in
--   let x, w1 = receive r1 in
--   x


data IntList = Nil | Cons Int IntList

null' :: IntList -> Bool
null' l =
  case l of
    Nil -> True
    Cons x y -> False
