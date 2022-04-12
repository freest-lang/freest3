type IntStream : SU = *!Int

type BitStream : SU = IntStream
type Random    : SU = dualof IntStream

-- Args -> *!SendType
genericUnSender : forall a:MU . a -> (rec b:SU . !a;b) -> ()
genericUnSender x chan =
    genericUnSender[a] x $ send x chan

receiveBits : Int -> dualof BitStream -> Int
receiveBits nBits bitS = 
    if nBits < 0
    then 0
    else
        let (i, bitS) = receive bitS in
        i + (receiveBits (nBits-1) bitS) * 2

initRandom : Random
initRandom =
    -- init bit sending 
    let (bitSend, bitRecv) = new BitStream in
    -- init bit sending threads
    fork $ genericUnSender[Int] 0 bitSend;
    fork $ genericUnSender[Int] 1 bitSend;
    -- init server/client endpoint
    let (client, server) = new Random in
    -- init random server
    fork $ genericUnSender[Int] (receiveBits 4 bitRecv) server;
    -- return client endpoint
    client

main : Bool
main =
    let rand = initRandom in
    let (i, _) = receive rand in
    --printIntLn i; --comment out this line to manually test 
    -- no way to test non-deterministic
    --   values, so opt instead to only
    --   check no errors are raised
    --   during execution
    True