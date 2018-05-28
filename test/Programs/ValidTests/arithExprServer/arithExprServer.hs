
-- type TermChan = +{Const: !Int, Add: TermChan;TermChan, Mult: TermChan;TermChan}
-- &{Const: ?Int, Add: TermChan;TermChan, Mult: TermChan;TermChan}


-- computeService :: (rec TermChan . &{Const: ?Int;Skip, Add: TermChan;TermChan, Mult: TermChan;TermChan};!Int) -> Skip
-- computeService c =
--   let n1, c1 = receiveEval[!Int;Skip] c in
--   send n1 c1

-- receiveEval :: forall x => (rec TermChan . &{Const: ?Int, Add: TermChan;TermChan, Mult: TermChan;TermChan};x) -> (Int, x)
-- receiveEval c =
--   match c with
--     Const c1 ->
--       let x, y = receive c1 in
--       (x,y)
--     Add c1 ->
--       let n1, c2 = receiveEval[rec TermChan . &{Const: ?Int, Add: TermChan;TermChan, Mult: TermChan;TermChan}] c1 in
--       let n2, c3 = receiveEval[rec TermChan . &{Const: ?Int, Add: TermChan;TermChan, Mult: TermChan;TermChan}] c2 in
--       (n1+n2, c3)
--     Mult c1 ->
--       let n1, c2 = receiveEval[rec TermChan . &{Const: ?Int, Add: TermChan;TermChan, Mult: TermChan;TermChan}] c1 in
--       let n2, c3 = receiveEval[rec TermChan . &{Const: ?Int, Add: TermChan;TermChan, Mult: TermChan;TermChan}] c2 in
--       (n1*n2, c3)

---- TESTING ---

-- client :: (rec TermChan . +{Const: !Int, Add: TermChan;TermChan, Mult: TermChan;TermChan});?Int;Skip -> (Int, Skip)
-- client c =
--   let c1 = select Add c in
--   let c2 = select Const c1 in
--   let c3 = send 5 c2 in
--   let c4 = select Const c3 in
--   let c5 = send 5 c4 in
--   receive c5



{- Just Const-}
-- parens
client :: (rec TermChan . +{Const: !Int, Add: TermChan;TermChan, Mult: TermChan;TermChan});?Int;Skip -> (Int, Skip)
client c =
  let c1 = select Const c in
  let c2 = send 5 c1 in
  receive c2
 

start :: Int
start = 10

---- END OF TESTING ---

-- start :: Int
-- start =
--   let w, r  = new (rec TermChan . +{Const: !Int, Add: TermChan;TermChan, Mult: TermChan;TermChan}) in
--   let x = fork (computeService w) in
--   let v, s = client r in
--   v



{-
client ::  (rec TermChan . +{Const: !Int, Add: TermChan;TermChan, Mult: TermChan;TermChan});?Int;Skip -> (Int, Skip)
client c =
  let c1 = select Add c in
  let c2 = select Const c1 in
  let c3 = send 5 c2 in
  let c4 = select Mult c3 in
  let c5 = select Const c4 in
  let c6 = send 7 c5 in
  let c7 = select Const c6 in
  let c8 = send 9 c7 in
  receive c8

-}
