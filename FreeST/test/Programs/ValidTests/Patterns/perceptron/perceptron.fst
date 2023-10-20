
-- input 
input : IntList
input = List 1 $ List 50 $ List 100 Nil

layers : IntList
layers = List 50 $ List 50 $ List 50 Nil

seed : Int
seed = 1

-- main
main : Int
main = 
    let ls = List (len input) layers in -- add connection from entry neurons
    let ls = addTail 1 ls            in -- add connection to exit neuron
    let cons = mkLayers ls           in -- create network
    -- printLayers cons; -- TODO remove PRINT
    let rs = startup input cons      in -- start up network
    -- putStrLn "Cenas e tal";
    let r  = recNeuron rs            in
    -- putStr "Result: ";
    -- printIntLn r
    r

---- Structures ----
data IntList = Nil | List Int IntList

data SList   = SNil | SList Send SList
data RList   = RNil | RList Receive RList

type Channels   = (SList,RList)
data Connection = CNil | Connection Channels Connection
data Layers     = LNil | Layers Connection Layers

---- Channels ----
type Send    = !Int
type Receive = dualof Send

---- Network ----

-- structure
mkLayers : IntList -> Layers
mkLayers Nil           = LNil
mkLayers (List n1 Nil) = LNil
mkLayers (List n1 (List n2 ns)) = Layers (mkLayer n1 n2) (mkLayers (List n2 ns))

mkLayer : Int -> Int -> Connection
mkLayer n1 n2 = 
    if n1 <= 0 then
        CNil
    else
        Connection (mkChannels n2) (mkLayer (n1-1) n2)

mkChannels : Int -> Channels
mkChannels n =
    if n <= 0 then
        (SNil,RNil)
    else
        let (ss,rs) = mkChannels (n-1) in
        let (s,r)   = new @Send ()     in
        (SList s ss, RList r rs)

-- startup
startup : IntList -> Layers -> RList
startup input LNil           = RNil
startup input (Layers l0 ls) = 
    mkNeurons0 input l0;
    startupN (Layers l0 ls)

startupN : Layers -> RList
startupN LNil             = RNil
startupN (Layers l1 LNil) = receiveCs l1
startupN (Layers l1 (Layers l2 ls)) =
    mkNeurons l1 l2;
    startupN (Layers l2 ls)

-- neurons
mkNeurons0 : IntList -> Connection -> ()
mkNeurons0 (List i input) (Connection c con0) =
    let (s,_) = c in
    fork (\_:() 1-> mkNeuron0 i s);
    mkNeurons0 input con0
mkNeurons0 _ _ = ()

mkNeuron0 : Int -> SList -> ()
mkNeuron0 input ss = sendNeuron input ss

mkNeurons : Connection -> Connection -> ()
mkNeurons con1 CNil = ()
mkNeurons con1 (Connection c con3) = 
    let (s1,_)  = c in
    let (r1,r2) = getHeads con1 in
    fork (\_:() 1-> mkNeuron r1 s1);
    mkNeurons r2 con3

mkNeuron : RList -> SList -> ()
mkNeuron rs ss = 
    let x = recNeuron rs in
    let x = if x < 50 then 0 else 1 in -- TODO random number
    sendNeuron x ss

recNeuron : RList -> Int
recNeuron RNil         = 0
recNeuron (RList r rs) = let (x,_) = receive r in 
                         x + recNeuron rs

sendNeuron : Int -> SList -> ()
sendNeuron x SNil = ()
sendNeuron x (SList s ss) = send x s; sendNeuron x ss

---- Auxiliary ----

-- generates a random integer between 0 and 99
genInt : Int -> Int
genInt n = genIntBounded n 0 100

-- generates a random integer 
 -- between l (lower, inclusive) and u (upper, exclusive) limits
genIntBounded : Int -> Int -> Int -> Int
genIntBounded n l u = mod ((n+1) * (mod 111 100)) (u-l) + l

-- calculates the size of an IntList
len : IntList -> Int
len Nil = 0
len (List _ xs) = 1 + len xs

-- adds int to the end of a list
addTail : Int -> IntList -> IntList
addTail x Nil         = List x Nil
addTail x (List e xs) = List e $ addTail x xs

-- gets last layers receive channels
receiveCs : Connection -> RList
receiveCs CNil = RNil
receiveCs (Connection c con) = let (_,rs) = c in
                               concatRList rs $ receiveCs con

-- concatenates RLists
concatRList : RList -> RList -> RList
concatRList RNil rs2          = rs2
concatRList (RList e rs1) rs2 = RList e $ concatRList rs1 rs2

-- gets the heads of the 
getHeads : Connection -> (RList,Connection)
getHeads CNil = (RNil, CNil)
getHeads (Connection c con) = 
    let (s,r) = c in
    let (heads,tails) = getHeads con in
    let (head ,tail ) = getHead r    in
    (concatRList head heads, Connection (s,tail) tails)

getHead : RList -> (RList,RList)
getHead RNil         = (RNil,RNil)
getHead (RList r rs) = (RList r RNil,rs) 

-- prints
printLayers : Layers -> ()
printLayers LNil           = ()
printLayers (Layers cs ls) = 
            printConnection cs;
            printLayers ls

printConnection : Connection -> ()
printConnection CNil = putStrLn ""
printConnection (Connection _ cs) = putStr "o "; printConnection cs
