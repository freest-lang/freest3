type Purchase = forall i:(bot,top), p:(i,top) => ?i() ; !p() ; Purchase

client : forall p1:(bot,top), p2:(p1,top) => Purchase ->[top,bot] Purchase 1->[p1,p2+2] Diverge
client =
    forall p1:(bot,top), p2:(p1,top) =>
    \item1: Purchase ->
    \item2: Purchase 1->
    let (_, pay1) = receive (item1{p1}{p1+2}) in
    let (_, pay2) = receive (item2{p2}{p2+2}) in
    let purchase1 = send () pay1 in
    let purchase2 = send () pay2 in
    (client{p1+1}{p2+1}) purchase1 purchase2

purchaseServer : forall p1_1:(bot,p1_2), p1_2:(p1_1,p2_1), p2_1:(p1_2,p2_2), p2_2:(p2_1,top) => dualof Purchase ->[top,bot] dualof Purchase 1->[p1_1,bot] dualof Purchase 1->[p1_1,bot] dualof Purchase 1->[p1_1,p2_2+2] Diverge
purchaseServer =
    forall p1_1:(bot,p1_2), p1_2:(p1_1,p2_1), p2_1:(p1_2,p2_2), p2_2:(p2_1,top) =>
    \cli1_item1: dualof Purchase ->
    \cli1_item2: dualof Purchase 1->
    \cli2_item1: dualof Purchase 1->
    \cli2_item2: dualof Purchase 1->
    let cli1_pay1 = send () (cli1_item1{p1_1}{p1_1+2}) in
    let (_, cli1_purchase1) = receive cli1_pay1 in
    let cli1_pay2 = send () (cli1_item2{p1_2}{p1_2+2}) in
    let (_, cli1_purchase2) = receive cli1_pay2 in
    let cli2_pay1 = send () (cli2_item1{p2_1}{p2_1+2}) in
    let (_, cli2_purchase1) = receive cli2_pay1 in
    let cli2_pay2 = send () (cli2_item2{p2_2}{p2_2+2}) in
    let (_, cli2_purchase2) = receive cli2_pay2 in
    (purchaseServer{p1_1+1}{p1_2+1}{p2_1+1}{p2_2+1}) cli1_purchase1 cli1_purchase2 cli2_purchase1 cli2_purchase2

main : ()
main =
    let (cli1_1, cli1_item1) = new @Purchase () in
    let (cli1_2, cli1_item2) = new @Purchase () in
    let (cli2_1, cli2_item1) = new @Purchase () in
    let (cli2_2, cli2_item2) = new @Purchase () in
    fork (\_:()1-> (purchaseServer{1}{2}{3}{4}) cli1_item1 cli1_item2 cli2_item1 cli2_item2);
    fork (\_:()1-> (client{1}{2}) cli1_1 cli1_2);
    (client{3}{4}) cli2_1 cli2_2