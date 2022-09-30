-- In datatype constructor types, all arrows except the first 
-- after a linear field should be 1->, otherwise we can use 
-- the linear fields unrestrictedly.

data UnSend = UnSend !Int () -- UnSend : !Int -> () 1-> UnSend
data UnRecv = UnRecv ?Int () -- UnRecv : ?Int -> () 1-> UnSend

unSend : Int -> (() -> UnSend) -> ()
unSend n us = case us () of {UnSend s _ -> send n s; ()}

unRecv : (() -> UnRecv) -> Int 
unRecv ur = case ur () of {UnRecv r _ -> let (n,_) = receive r in n}

main : Int
main = let (s, r) = new !Int in 
       let us = UnSend s in -- us : () 1-> UnSend
       let ur = UnRecv r in -- ur : () 1-> UnRecv
       unSend 5 us;
       unSend 6 us; -- use twice (wrong: should not be in scope)
       unRecv ur;
       unRecv ur;
       unRecv ur    -- use thrice (wrong: should not be in scope)