-- internal

type Tail : SL = {- Enqueue: -} !Int; ?Request; Tail
type Head : SL = {- Dequeue: -} &{Just: ?Int, Nothing: Skip}; ?Request; Head

type InternalHead : SL = !dualof Head

type Request : SU = *+{Tail, Head, Prev}


runSingletonNode : Bool -> Int -> dualof Tail -> dualof Head -o (Request, dualof Request) -o ()
runSingletonNode isEmpty i tail head requests =
    match snd[Request, dualof Request] requests with {
        Tail _ -> -- someone asked for Tail to be served [ENQUEUE]
            let (j, tail) = receive tail in
            if isEmpty
            then
                -- queue is 'empty', becomes a singleton
                let tail = send (fst[Request, dualof Request] requests) tail in
                runSingletonNode False j tail head requests
            else
                -- queue is a singleton, ...
                -- (prepare channels)
                let (c , s ) = new InternalHead in
                let (requestOut , requestIn ) = requests in
                let (requestOut', requestIn') = new Request in

                let toNext   = (requestOut , c ) in
                let fromPrev = s  in

                -- ... creates a head node ...
                fork $ runHeadNode i head toNext (requestOut', requestIn');
                -- ... and continues as a tail node
                let tail = send (fst[Request, dualof Request] requests) tail in
                runTailNode j tail fromPrev requests,
        Head _ -> -- someone asked for Head to be served [DEQUEUE]
            if isEmpty
            then 
                -- queue is 'empty', continues 'empty'
                let head = select Nothing head & send (fst[Request, dualof Request] requests) in
                runSingletonNode True i tail head requests
            else
                -- queue is a singleton, becomes 'empty'
                let head = select Just head & send i & send (fst[Request, dualof Request] requests) in
                runSingletonNode True i tail head requests
  }
--


runTailNode : Int -> dualof Tail -> dualof InternalHead -o (Request, dualof Request) -o ()
runTailNode i tail fromPrev requests = 
    match snd[Request, dualof Request] requests with {
        Tail _ -> -- someone asked for Tail to be served [ENQUEUE]
            -- node receives a new value, ...
            let (j, tail) = receive tail in
            -- (prepare channels)
            let (c, s) = new InternalHead in
            let (requestOut, requestIn) = new Request in

            let fromPrev' = s in
            let toNext    = (requestOut, c) in
            let requests' = (requestOut, requestIn) in
            -- ... creates a new tail node with the new value ...
            let tail = send requestOut tail in
            fork $ runTailNode j tail fromPrev' requests';
            -- ... and continues as a mid node 
            runMidNode i fromPrev toNext requests,
        Prev _ -> -- someone asked for InternalHead to be served [BECOME NEW HEAD]
            -- node receives the head, ...
            let head = fst[dualof Head, Skip] $ receive fromPrev in
            -- ... runs as a singleton node
            runSingletonNode False i tail head requests
    }
--


runMidNode : Int -> dualof InternalHead -> (Request, InternalHead) -o (Request, dualof Request) -o ()
runMidNode i fromPrev toNext requests = 
    match snd[Request, dualof Request] requests with {
        Prev _ -> -- someone asked for InternalHead to be served [BECOME NEW HEAD]
            -- node receives the head, ...
            let head = fst[dualof Head, Skip] $ receive fromPrev in
            -- ... runs as a head node
            runHeadNode i head toNext requests
    }

runHeadNode : Int -> dualof Head -> (Request, InternalHead) -o (Request, dualof Request) -o ()
runHeadNode i head toNext requests = 
    match snd[Request, dualof Request] requests with {
        Head _ -> -- someone asked for Head to be served [DEQUEUE]
            let (nextReq, next) = toNext in
            -- node sends value, ...
            let head = select Just head & send i & send nextReq in
            -- ... tells next node to become the new tail 
            let _ = select Prev nextReq in
            let _ = send head next in
            ()
    }

-- queue

type Queue = ((Tail, Request), (Head, Request))

initQueue : Queue
initQueue = 
    let (tailC, tailS) = new Tail in
    let (headC, headS) = new Head in
    let (reqC , reqS ) = new Request in
    fork $ runSingletonNode True 0 tailS headS (reqC, reqS);
    ((tailC, reqC), (headC, reqC))

-- helper functions

data MaybeInt = JustInt Int | NothingInt

enqueue : Int -> Queue -> Queue
enqueue i queue =
    let (tail', head') = queue in
    let (tail, req) = tail' in
    let _ = select Tail req in
    let (req, tail) = send i tail & receive in
    ((tail, req), head')

dequeue : Queue -> (MaybeInt, Queue)
dequeue queue = 
    let (tail', head') = queue in
    let (head, req) = head' in
    let _ = select Head req in
    match head with {
        Just head -> 
            let (i  , head) = receive head in
            let (req, head) = receive head in
            (JustInt i, (tail', (head, req))),
        Nothing head ->
            let (req, head) = receive head in
            (NothingInt, (tail', (head, req)))
    }

-- main

-- main : ()
-- main = 
--     let queue = initQueue in
--     let queue = enqueue 2 $ enqueue 1 queue in
--     let (maybeInt, queue) = dequeue queue in
--     case maybeInt of {
--         JustInt i -> printIntLn i
--     }