-- | The client view of a linear interaction with a bag (multiset) of integer values
type Bag:1S = +{Put: !Int, Get: ?Int}
  
-- | The client view of a shared interaction with a bag
type SharedBag = *?Bag

-- | The state of the shared bag: integer messages in transit
type State = (*?Int, *!Int)

-- Server side

-- | An empty shared bag
emptyBagServer : dualof SharedBag -> Diverge
emptyBagServer = bagServer (new *?Int)

-- | A shared bag server with a state
bagServer : State -> dualof SharedBag -> Diverge
bagServer state serverChannel =
  let (clientSide, serverSide) = new Bag in
  send clientSide serverChannel;
  fork $ handleClient state serverSide;
  bagServer state serverChannel

-- | Handling a linear interaction with a particular client
handleClient : State -> dualof Bag -> ()
handleClient state chan =
  let (readFromState, writeOnState) = state in
  match chan with
    { Get chan -> let (n, _) = receive readFromState in send n chan; ()
    , Put chan -> let (n, _) = receive chan in send n writeOnState; ()
    }

-- Client side, utilities

-- | Put an integer on a shared bag
put : Int -> SharedBag -> ()
put n q =
  let (c, _) = receive q in
  let c = select Put c in
  send n c;
  ()

-- | Get an integer from a shared bag
get : SharedBag -> Int
get q =
  let (c, _) = receive q in
  let c = select Get c in
  let (n, _) = receive c in
  n

-- An application

-- | Put three numbers and get two; return the sum
main : Int
main =
  let (clientSide, serverSide) = new SharedBag in
  fork $ emptyBagServer serverSide;
  fork $ put 7 clientSide;
  fork $ put 5 clientSide;
  fork $ put 1 clientSide;
  get clientSide + get clientSide
