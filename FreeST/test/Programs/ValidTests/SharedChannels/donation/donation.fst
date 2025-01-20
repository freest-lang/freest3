{-
Based on the Donation.pi from SePi, available in 
    http://rss.di.fc.ul.pt/tryit/SePi#

Note that FreeST does not have refinement types
-}

-- 1. Type abbreviations that make the code below more understandable
type CreditCard = String

-- type Promotion = *?(String, CreditCard, Int)
type Promotion  = *?Promotion'
type Promotion' = !String; !CreditCard; !Int; Close 

type Decision = &{ Accepted: ?Promotion; Close
                 , Denied  : ?String   ; Close
                 }

type DonationS = *!Donation
type Donation  = +{ SetTitle: !String; Donation
                  , SetDate : !Int   ; Donation
                  , Commit  : Decision
                  }

type Fork = *+{Over}
type Join = dualof Fork

donate : Promotion -> String -> CreditCard -> Int -> ()
donate p donor ccard amount =
  receive_ @Promotion' p |> send donor |> send ccard |> send amount |> close

-- 2. One possible client
helpSavingTheWolf : dualof DonationS -> ()
helpSavingTheWolf donationServer =
  match
    donationServer |> receive_ @Donation |>
    select SetDate |> send 2012 |>                    -- setup the date
    select SetTitle |> send "Help Saving the Wolf" |> -- setup the title
    select SetDate |> send 2013 |>                    -- fix the 2012 date
    select Commit
  with {                        -- wait for the outcome
    Accepted p ->               -- if accepted, we have two benefactors
      let d = receiveAndClose @Promotion p in 
      fork (\_:()1-> donate d "Benefactor1" "2345" 5);
      fork (\_:()1-> donate d "Benefactor2" "1234" 20);
      donate d "Benefactor3" "1004" 10,
    Denied p ->                 -- otherwise, print the reason
      putStrLn $ receiveAndClose @String p
  }

wrongYear : dualof DonationS -> ()
wrongYear donationServer = 
  let (p, donationServer) = receive donationServer in     -- get a session
  match
    p |>
    select SetDate |> send 1999 |>          -- setup the date
    select SetTitle |> send "Help Saving the Mink" |> -- setup the title
    select Commit
  with {                        -- wait for the outcome
    Accepted p -> -- Not going to happen...
      let d = receiveAndClose @Promotion p in 
      donate d "No Benefactor" "0000" 0,
    Denied p ->
      putStrLn $ receiveAndClose @String p
  }


-- 3. The bank that charges credit cards
bank : CreditCard -> Int -> ()
bank ccard amount =
  putStrLn $ "Charging " ^^ show @Int amount ^^ " euros on card " ^^ ccard


-- 4. The Online Donation Server
promotion : dualof Promotion -> ()
promotion p =
  let (c, p') = new @Promotion' () in
  let p = send c p in
  let (donor, p') = receive p' in
  let (ccard, p') = receive p' in
  let amount      = receiveAndWait @Int p' in
  bank ccard amount ;
  promotion p

setup : String -> Int -> Fork -> dualof Donation 1-> ()
setup title date f (SetDate  p) = let (d, p) = receive p in setup title d f p
setup title date f (SetTitle p) = let (t, p) = receive p in setup t date f p
setup title date f (Commit   p) =
  (if date < 2013
  then 
    select Denied p |> send "Can only accept donations from 2013" |> wait
  else 
    let (c, s) = new @Promotion () in
    select Accepted p |> send c |> wait ;
    promotion s) ;
  select Over f ; ()

waitFor : Int -> Join -> ()
waitFor n join =
  putStrLn $ "waitFor " ^^ show @Int n ;
  if n == 0 then ()
  else match join with { Over _ -> waitFor (n - 1) join }

donationServer : Int -> Int -> DonationS -> (Fork, Join) 1-> ()
donationServer k n donationService fj =
  if k == 0 then waitFor n (snd @Fork @Join fj)
  else
    let (p1, p2) = new @Donation () in        -- create a channel for a new donation campaign
    let donationService = send p1 donationService in  -- send one end;  keep the other (p2)
    fork (\_:() 1-> setup "Help me" 2000 (fst @Fork @Join fj) p2); -- call with default values
    donationServer (k - 1) n donationService fj    -- serve another client

-- 5. Main
main : ()
main = 
  let (ps1, ps2) = new @DonationS () in   -- create a Online Donation channel
  fork (\_:() 1-> helpSavingTheWolf ps2); -- let the whole world know the other
  fork (\_:() 1-> wrongYear ps2);
  fork (\_:() 1-> helpSavingTheWolf ps2);
  donationServer 7 7 ps1 (new @Fork ())   -- send one end to the Donation Server
