-- If a datatype contains a linear field, then it must be linear,
-- otherwise that field can be used unrestrictedly.

data UnSend = UnSend !Int
data UnRecv = UnRecv ?Int

unSend : Int -> UnSend -> ()
unSend n us = case us of {UnSend s -> send n s; ()}

unRecv : UnRecv -> Int 
unRecv ur = case ur of {UnRecv r -> let (n,_) = receive r in n}

main : Int
main = let (s, r) = new @!Int () in 
       let us = UnSend s in -- us : UnSend (linear)
       let ur = UnRecv r in -- ur : UnSend (linear)
       unSend 5 us;
       unSend 6 us; -- use twice (wrong: should be out of scope)
       unRecv ur;
       unRecv ur;
       unRecv ur   -- use thrice (wrong: should be out of scope)