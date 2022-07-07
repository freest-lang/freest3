
-- input 
input : IntList
input = List 1 $ List 50 $ List 100 End

layers : IntList
layers = List 50 $ List 50 $ List 50 End

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
    -- printStringLn "Cenas e tal";
    let r  = recNeuron rs            in
    -- printString "Result: ";
    -- printIntLn r
    r

---- Structures ----
data IntList = End | List Int IntList

data SList   = SEnd | SList Send SList
data RList   = REnd | RList Receive RList

type Channels   = (SList,RList)
data Connection = CEnd | Connection Channels Connection
data Layers     = LEnd | Layers Connection Layers

---- Channels ----
type Send    : SL = !Int
type Receive : SL = dualof Send

---- Network ----

-- structure
mkLayers : IntList -> Layers
mkLayers ns = 
    case ns of {
        End -> LEnd,
        List n1 ns ->
            case ns of {
                End -> LEnd,
                List n2 _ ->
                    Layers (mkLayer n1 n2) (mkLayers ns)
            }
    }

mkLayer : Int -> Int -> Connection
mkLayer 0  n2 = CEnd
mkLayer n1 n2 = Connection (mkChannels n2) (mkLayer (n1-1) n2)

mkChannels : Int -> Channels
mkChannels 0 = (SEnd,REnd)
mkChannels n = 
    let (ss,rs) = mkChannels (n-1) in
    let (s,r)   = new Send         in
    (SList s ss, RList r rs)

-- startup
startup : IntList -> Layers -> RList
startup input ls =
    case ls of {
        LEnd -> REnd,
        Layers l0 _ -> 
            mkNeurons0 input l0;
            startupN  ls
    }

startupN : Layers -> RList
startupN ls = 
    case ls of {
        LEnd -> REnd,
        Layers l1 ls -> 
            case ls of {
                LEnd -> receiveCs l1,
                Layers l2 _ -> 
                    mkNeurons l1 l2;
                    startupN ls
            }
    }

-- neurons
mkNeurons0 : IntList -> Connection -> ()
mkNeurons0 input con0 =
    case input of {
        End -> (),
        List i input -> 
            case con0 of {
                CEnd -> (),
                Connection c con0 -> 
                    let (s,_) = c in
                    fork $ mkNeuron0 i s;
                    mkNeurons0 input con0
            }
    }

mkNeuron0 : Int -> SList -> ()
mkNeuron0 input ss = sendNeuron input ss

mkNeurons : Connection -> Connection -> ()
mkNeurons con1 con2 =
    case con2 of {
        CEnd -> (),
        Connection c con3 -> 
            let (s1,_)  = c in
            let (r1,r2) = getHeads con1 in
            fork $ mkNeuron r1 s1;
            mkNeurons r2 con3
    }

mkNeuron : RList -> SList -> ()
mkNeuron rs ss = 
    let x = recNeuron rs in
    let x = if x < 50 then 0 else 1 in -- TODO random number
    sendNeuron x ss

recNeuron : RList -> Int
recNeuron rs =
    case rs of {
        REnd -> 0,
        RList r rs ->
            let (x,_) = receive r in
            x + recNeuron rs
    }

sendNeuron : Int -> SList -> ()
sendNeuron x ss =
    case ss of {
        SEnd -> (),
        SList s ss ->
            send x s;
            sendNeuron x ss
    }

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
len xs =
    case xs of {
        End -> 0,
        List _ xs -> 1 + len xs
    }

-- adds int to the end of a list
addTail : Int -> IntList -> IntList
addTail x xs =
    case xs of {
        End -> List x End,
        List e xs -> 
            List e $ addTail x xs
    }

-- gets last layers receive channels
receiveCs : Connection -> RList
receiveCs con =
    case con of {
        CEnd -> REnd,
        Connection c con ->
            let (_,rs) = c in
            concatRList rs $ receiveCs con
    }

-- concatenates RLists
concatRList : RList -> RList -> RList
concatRList rs1 rs2 =
    case rs1 of {
        REnd -> rs2,
        RList e rs1 -> 
            RList e $ concatRList rs1 rs2
    }

-- gets the heads of the 
getHeads : Connection -> (RList,Connection)
getHeads con =
    case con of {
        CEnd -> (REnd, CEnd),
        Connection c con ->
            let (s,r) = c in
            let (heads,tails) = getHeads con in
            let (head ,tail ) = getHead r    in
            (concatRList head heads, Connection (s,tail) tails)
    }

getHead : RList -> (RList,RList)
getHead rs =
    case rs of {
        REnd -> (REnd,REnd),
        RList r rs -> (RList r REnd,rs)
    }

-- prints
printLayers : Layers -> ()
printLayers ls = 
    case ls of {
        LEnd -> (),
        Layers cs ls -> 
            printConnection cs;
            printLayers ls
    }

printConnection : Connection -> ()
printConnection cs = 
    case cs of {
        CEnd -> printStringLn "",
        Connection _ cs ->
            printString "o ";
            printConnection cs
    }