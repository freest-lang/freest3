type A = Close 1
type B = Close 2
type C = Close 3
type D = Close 4
type E = Close 5
type F = Close 6

func1 : A 1->[top,bot] B 1->[1,bot] C 1->[1,bot] D 1->[1,bot] E 1->[1,bot] F 1->[1,6] ()
func1 a b c d e f = 
    close a;
    close b;
    close c;
    close d;
    close e;
    close f

func2 : dualof A 1->[top,bot] dualof B 1->[1,bot] dualof C 1->[1,bot] dualof D 1->[1,bot] dualof E 1->[1,bot] dualof F 1->[1,6] ()
func2 a b c d e f = 
    wait a;
    wait b;
    wait c;
    wait d;
    wait e;
    wait f

main : ()
main = 
    let (a, aDual) = new @A () in
    let (b, bDual) = new @B () in
    let (c, cDual) = new @C () in
    let (d, dDual) = new @D () in
    let (e, eDual) = new @E () in
    let (f, fDual) = new @F () in
    fork @() (\_:() 1-> func1 a b c d e f);
    func2 aDual bDual cDual dDual eDual fDual