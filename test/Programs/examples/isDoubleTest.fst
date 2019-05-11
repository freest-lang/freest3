-- main : Bool
-- main =
--   let s, r = new !Int in
--   let _ = fork (sender s 10) in
--   if (div (f r) 2) == 5 then
--     True
--   else
--     False

-- f : ?Int -> Int
-- f c = let x, c = receive c in x

-- sender : !Int -> Int -> Skip
-- sender c i = send c (i * 2)
-- --  let _ = send c (i * 2) in ()

g : Int 
g =
  let _ = fork () in a

a : Int
a =
  let x = g in x 

main : Int
main = a
-- f1 : !Int;!Int -> Skip
-- f1 c = send (send c 5) 5

-- f2 : ?Int;?Int -> Int
-- f2 c =
--   let x1, c = receive c in
--   let x2, c = receive c in
--   x1 + x2
