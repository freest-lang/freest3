{-
Based on the Donation.pi from SePi, available in 
    http://rss.di.fc.ul.pt/tryit/SePi#

Note that FreeST does not have refinement types
-}

-- 1. Type abbreviations that make the code below more understandable
type CreditCard = String

-- type Promotion : *S = *?(String, CreditCard, Int)
type Promotion  : *S = *?Promotion'
type Promotion' : 1S = !String; !CreditCard; !Int; End 

type Decision : 1S = &{ Accepted: ?Promotion; End
                      , Denied  : ?String   ; End
                      }

type DonationS : *S = *!Donation
type Donation : 1S = +{ SetTitle: !String; Donation
                      , SetDate : !Int   ; Donation
                      , Commit  : Decision
                      }


-- 2. One possible client
helpSavingTheWolf : dualof DonationS -> ()
helpSavingTheWolf donationServer =
    let (p, donationServer) = receive donationServer in         -- get a session channel p from the donation server 
    let p = select SetDate  p |> send 2012 in                    -- setup the date
    let p = select SetTitle p |> send "Help Saving the Wolf" in  -- setup the title
    let p = select SetDate  p |> send 2013 in                    -- fix the 2012 date
    let p = select Commit p in                                  -- commit once happy
    match p with {                                              -- wait for the outcome
        Accepted p ->                                           -- if accepted, we have three benefactors
            let d = receiveAndClose @Promotion p in 
            fork (\_:()1-> donate d "Benefactor1" "2345" 5);
            fork (\_:()1-> donate d "Benefactor2" "1234" 20);
            donate d "Benefactor3" "1004" 10,
        Denied p ->                                             -- otherwise, print the reason
            printStringLn $ receiveAndClose @String p
    }

donate : Promotion -> String -> CreditCard -> Int -> ()
donate p donor ccard amount =
    -- let _ = send (donor, ccard, amount) p in ()
    let (p, _) = receive p in
    send donor p |> send ccard |> send amount |> close


-- 3. The bank that charges credit cards
bank : CreditCard -> Int -> ()
bank ccard amount =
    -- printStringLn $ "Charging " ++ show amount ++ " euros on card " ++ ccard
    printString "Charging "; printInt amount; printString " euros on card "; printStringLn ccard


-- 4. The Online Donation Server
donationServer : DonationS -> ()
donationServer donationService =
    let (p1, p2) = new Donation in                      -- create a channel for a new donation campaign
    let donationService = send p1 donationService in    -- send one end; keep the other (p2)
    fork (\_:() 1-> setup p2 "Help me" 2000);           -- call with default values
    donationServer donationService                      -- serve another client

promotion : dualof Promotion -> ()
promotion p =
    --
    let (c, p') = new Promotion' in
    let p = send c p in
    --
    let (donor , p') = receive p' in
    let (ccard , p') = receive p' in
    let amount       = receiveAndClose @Int p' in
    bank ccard amount;
    promotion p

setup : dualof Donation -> String 1-> Int 1-> ()
setup p title date =
    match p with {
        SetDate  p -> let (d, p) = receive p in setup p title d   ,
        SetTitle p -> let (t, p) = receive p in setup p t     date,
        Commit   p -> if date < 2013
                      then 
                        select Denied   p |> send "We can only accept 2013 donations\n" |> close
                      else 
                        let (c, s) = new Promotion in
                        select Accepted p |> send c |> close ;
                        promotion s
    }


-- 5. Main
main : ()
main = 
    let (ps1, ps2) = new DonationS in  -- create a Online Donation channel
    fork (\_:() 1-> helpSavingTheWolf ps2);      -- let the whole world know the other
    fork (\_:() 1-> helpSavingTheWolf ps2);
    fork (\_:() 1-> helpSavingTheWolf ps2);
    donationServer ps1                 -- send one end to the Donation Server
