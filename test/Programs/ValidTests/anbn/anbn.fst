{-
The language of S0 is {A^nB^n | n >= 1}

S0 -> a S1
S1 -> a S1 b | b

type S0 = +{A: S1}
type S1 = +{A: S1; +{B: Skip}, B: Skip}

type S0 = +{A: rec x:SL. +{A: x; +{B: Skip}, B: Skip}} 
type S1 = (rec x:SL. +{A: x; +{B: Skip}, B: Skip})

-}

-- client : Int -> S0 -> Skip
client : Int -> +{A: rec x:SL. +{A: x; +{B: Skip}, B: Skip}} -> Skip
client n c =
  let c = select A c in
  client'[Skip] (n - 1) c

-- client' : Int -> S1;α -> α
client' : forall α : SL => Int -> (rec x:SL. +{A: x; +{B: Skip}, B: Skip}); α -> α
client' n c =
  if n == 0
  then
    select B c                                  -- α
  else
    let c = select A c in                       -- S1; +{B: Skip}; α
    let c = client'[+{B: Skip}; α] (n - 1) c in -- +{B: Skip}; α
    select B c                                  -- α

server : &{A: rec x:SL. &{A: x; &{B: Skip}, B: Skip}} -> Skip
server c =
  match c with {
    A c -> server'[Skip] c
  }

-- server' : forall α : SL => (rec x:SL. &{A: x; &{B: Skip}}); &{B: Skip}; α -> α
server' : forall α : SL => (rec x:SL. &{A: x; &{B: Skip}, B: Skip}); α -> α
server' c =
  match c with {
    A c ->               -- (rec x:SL. &{A: x; &{B: Skip}, B: Skip})) ; &{B: Skip}
      (let c = server'[&{B: Skip}; α] c in  -- &{B: Skip}; α
       match c with {
         B c -> c
       });      -- α
    B c ->               -- α
      c
  }

main : ()
main =
  let w, r = new +{A: rec x:SL. +{A: x; +{B: Skip}, B: Skip}} in
  let t = fork (client 25 w) in
  let r = server r in
  ()

