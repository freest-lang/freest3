type X = forall a : (bot, top) => !a(); !a+2(); X
type Y = forall b : (bot, top) => ?b(); Y

sender : forall p:(bot,top) => X ->[top,bot] Y 1->[p,x+2] ()
sender = 
    forall p:(bot,top) =>
    \x: X ->
    \y: Y 1->
    let x = send () (inst x) in 
    let (_, y) = receive (inst y) in 
    let x = send () x in 
    (sender{priority x}) x y 

receiver : forall p:(bot,top) => dualof X ->[top,x+2] ()
receiver = 
    forall p:(bot,top) =>
    \x: dualof X ->
    let (_, x) = receive (inst x) in 
    let (_, x) = receive x in 
    (receiver{priority x}) x

other : forall p:(bot,top) => dualof Y ->[top,y] ()
other = 
    forall p:(bot,top) =>
    \y: dualof Y ->
    let y = send () (inst y) in 
    (other{priority y}) y 

main : ()
main = 
    let (a,b) = new @X {1,3} () in 
    let (c,d) = new @Y {2,3} () in 
    fork (\_:() 1-> (sender{priority a}) a c);
    fork (\_:() 1-> (other{priority d}) d);
    (receiver{priority b}) b