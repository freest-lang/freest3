{-
Based on the Donation.pi from SePi, available in 
    http://rss.di.fc.ul.pt/tryit/SePi#

Note that FreeST does not have refinement types
-}

type CreditCard = String
type Amount = Int
type Date = Int

-- type PromotionS = *?(String, CreditCard, Int)
type PromotionS  = *?Promotion
type Promotion = !String ; !CreditCard ; !Amount ; Close 

type Decision = &{ Accepted: ?PromotionS; Close
                 , Denied  : ?String   ; Close
                 }

type DonationS = *!Donation
type Donation  = +{ SetTitle: !String ; Donation
                  , SetDate : !Date   ; Donation
                  , Commit  : Decision
                  }

-- 1. Fork join

type Fork = *+{Over}
type Join = dualof Fork

waitFor : Int -> Join -> ()
waitFor n join =
  if n == 0 then ()
  else match join with { Over _ -> waitFor (n - 1) join }


-- 2. Two clients

donate : PromotionS -> String -> CreditCard -> Amount -> ()
donate p donor ccard amount =
  receive_ @Promotion p |> send donor |> send ccard |> send amount |> close

helpSavingTheWolf : dualof DonationS -> ()
helpSavingTheWolf d =
  match
    d |> receive_ @Donation |>
    select SetDate |> send 2012 |>                    -- setup the date
    select SetTitle |> send "Help Saving the Wolf" |> -- setup the title
    select SetDate |> send 2013 |>                    -- fix the 2012 date
    select Commit
  with {
    Accepted d ->
      let p = receiveAndClose @PromotionS d in 
      fork (\_:()1-> donate p "Benefactor1" "2345" 5);
      fork (\_:()1-> donate p "Benefactor2" "1234" 20);
      donate p "Benefactor3" "1004" 10,
    Denied d ->
      putStrLn $ receiveAndClose @String d
  }

wrongYear : dualof DonationS -> ()
wrongYear d = 
  match
    d |> receive_ @Donation |>
    select SetDate |> send 1999 |>  -- wrong date
    select SetTitle |> send "Help Saving the Mink" |>
    select Commit
  with {
    Accepted d -> -- Not going to happen...
      let p = receiveAndClose @PromotionS d in 
      donate p "No Benefactor" "0000" 0,
    Denied d ->
      putStrLn $ receiveAndClose @String d
  }


-- 3. The bank that charges credit cards
charge : CreditCard -> Amount -> ()
charge ccard amount =
  putStrLn $ "Charging " ^^ show @Int amount ^^ " euros on card " ^^ ccard


-- 4. The Online Donation Server
promotion : Int -> Fork -> dualof PromotionS -> ()
promotion k f pc =
  if k == 0 then select Over f ; ()
  else
    let p = accept @Promotion pc in
    let (donor, p) = receive p in
    let (ccard, p) = receive p in
    let amount     = receiveAndWait @Int p in
    charge ccard amount ;
    promotion (k - 1) f pc

setup : String -> Date -> Fork -> PromotionS -> dualof Donation -> ()
setup title _ f p (SetDate  d) = let (date,  d) = receive d in setup title date f p d
setup _  date f p (SetTitle d) = let (title, d) = receive d in setup title date f p d
setup title date f p (Commit d) =
  (if date < 2013
  then 
    select Denied d |> send "Can only accept donations from year 2013"
  else 
    select Accepted d |> send p)
  |> wait ;
  select Over f ; ()

server : Int -> Int -> (Fork, Join) -> PromotionS -> DonationS -> ()
server k n fj p ds =
  if k == 0 then waitFor n (snd @Fork @Join fj)
  else
    let d = accept @Donation ds in
    fork (\_:() 1-> setup "<default>" 0000 (fst @Fork @Join fj) p d) ;
    server (k - 1) n fj p ds

donationServer : Int -> Int -> DonationS -> ()
donationServer noOfClients noOfDonations ds =
  let (f, j) = new @Fork () in
  let p = forkWith @PromotionS @() (promotion noOfDonations f) in
  server noOfClients noOfClients (new @Fork ()) p ds ;
  match j with { Over _ -> () }

-- 5. Main
main : ()
main = 
  let (ds, dc) = new @DonationS () in
  let noOfClients = 3 in
  let noOfDonations = 6 in
  fork (\_:() 1-> helpSavingTheWolf dc);
  fork (\_:() 1-> wrongYear dc);
  fork (\_:() 1-> helpSavingTheWolf dc);
  donationServer noOfClients noOfDonations ds
