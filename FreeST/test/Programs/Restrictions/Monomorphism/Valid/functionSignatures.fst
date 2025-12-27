type A = Close a
type B = Close b
type C = Close c
type D = Close d
type E = Close e
type F = Close f

func1 : A 1->[top,bot] B 1->[a,bot] C 1->[a,bot] D 1->[a,bot] E 1->[a,bot] F 1->[a,f] ()
func1 a b c d e f = 
    close a;
    close b;
    close c;
    close d;
    close e;
    close f

func2 : dualof A 1->[top,bot] dualof B 1->[a,bot] dualof C 1->[a,bot] dualof D 1->[a,bot] dualof E 1->[a,bot] dualof F 1->[a,f] ()
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