

-- | Prelude

id : forall a . a -> a
id = \\a => \x:a -> x

flip : forall a b c . (a -> b -> c) -> b -> a -> c
flip f x y = f y x

until : forall a . (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until[a] p f (f x)

-- | 'curry' converts an uncurried function to a curried function.
-- curry fst 1 2
-- 1

curry : forall a b c . ((a, b) -> c) -> a -> b -> c
curry f x y =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
-- uncurry (+) (1,2)
-- 3

uncurry  : forall a b c . (a -> b -> c) -> ((a, b) -> c)
uncurry f p =  f (fst[a,b] p) (snd[a,b] p)

-- | Swap the components of a pair.
swap : forall a b . (a,b) -> (b,a)
swap x = let (a,b) = x in (b,a)

-- |  Fixed-point Z combinator
fix : forall a . ((a -> a) -> (a -> a)) -> (a -> a)
fix f =
  (\x:(rec b.b -> (a -> a)) -> f (\z:a -> x x z))
  (\x:(rec b.b -> (a -> a)) -> f (\z:a -> x x z))

-- | A mark for functions that do not terminate
type Diverge = ()

-- | A function that diverges
diverge : () --Diverge
diverge = diverge

-- | Discard an unrestricted value
sink : forall a . a -> ()
sink _ = ()

-- | Execute a thunk n times, sequentially
repeat : forall a . Int -> (() -> a) -> ()
repeat n thunk =
    if n < 0
    then ()
    else 
        thunk ();
        repeat [a] (n - 1) thunk

-- | Receive a value from a star channel
receive_ : forall a:TL . *?a -> a
receive_ ch = fst [a, *?a] $ receive ch

-- | Send a value on a star channel
send_ : forall a:TL . a -> *!a -o ()
send_ x ch = sink [*!a] $ send x ch

-- | Fork n identical threads
parallel : Int -> (() -> ()) -> ()
parallel n thunk = repeat [()] n (\_:() -> fork (thunk ()))

-- | Create a new child process and a linear channel through which it can 
--   communicate with its parent process. Return the channel endpoint.
forkWith : forall a:SL . (dualof a -o ()) -> a
forkWith f =
    let (x, y) = new a in
    fork $ f y;
    x

-- |Session initiation
-- |Accept a request for a linear session on a shared channel.
-- |The requester uses a conventional receive to obtain the channel end
accept : forall a:SL b:SU . !a; b -> dualof a
accept ch =
    let (x, y) = new a in
    send x ch;
    y

-- |Session initiation on an start channel
accept_ : forall a:SL . *!a -> dualof a
accept_ ch = accept [a, *!a] ch

-- | Run a server process given its endpoint, a function to serve a client (a
-- handle) and the initial state.
runServer : forall a:SL b . (b -> dualof a -o b) -> b -> *!a -> () --Diverge
runServer handle state ch =
    runServer [a, b] handle (handle state (accept_ [a] ch)) ch 