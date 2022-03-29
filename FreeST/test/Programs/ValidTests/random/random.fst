type IntStream : SU = !Int; IntStream

type BitStream : SU = IntStream
type Random    : SU = dualof IntStream

zeroSender : BitStream -> ()
zeroSender ch = zeroSender $ send 0 ch

oneSender : BitStream -> ()
oneSender ch = oneSender $ send 1 ch

receiveInt : dualof BitStream -> Int
receiveInt bitS = receiveInt' 32 bitS

receiveInt' : Int -> dualof BitStream -> Int
receiveInt' nBits bitS = 
    if nBits < 0
    then 0
    else
        let (i, bitS) = receive bitS in
        i + (receiveInt' (nBits-1) bitS) * 2

randomServer : dualof BitStream -> dualof Random -> ()
randomServer bitS outS =
    randomServer bitS $ send (receiveInt bitS) outS

initRandom : Random
initRandom =
    -- init bit sending 
    let (sending, receiving) = new BitStream in
    -- init bit sending threads
    fork $ zeroSender sending;
    fork $ oneSender  sending;
    -- init server/client endpoint
    let (client, server) = new Random in
    -- init random server
    fork $ randomServer receiving server;
    -- return client endpoint
    client

main : Bool
main =
    let rand = initRandom in
    let (i, _) = receive rand in
    -- no way to test non-deterministic
    --   values, so opt instead to only
    --   check no errors are raised
    --   during execution
    True