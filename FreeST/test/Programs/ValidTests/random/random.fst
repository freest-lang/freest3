type IntStream : SU = *!Int

type BitStream : SU = IntStream
type Random    : SU = dualof IntStream

-- Args -> (Args -> SendType) -> *!SendType
genericSender : forall a b:ML c:SU . a -> (a -> b) -> (rec c:SU . !b;c) -> ()
genericSender arg f chan =
    genericSender[a, b, c] arg f $ send (f arg) chan

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
    fork $ genericSender[Int, Int, BitStream] 0 (id[Int]) bitSend;
    fork $ genericSender[Int, Int, BitStream] 1 (id[Int]) bitSend;
    -- init server/client endpoint
    let (client, server) = new Random in
    -- init random server
    fork $ genericSender[dualof BitStream, Int, dualof Random] bitRecv (receiveBits 4) server;
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