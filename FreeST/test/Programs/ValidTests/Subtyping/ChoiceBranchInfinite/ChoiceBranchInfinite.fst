type CB1 : 1S = +{A: !Int};&{A: ?Int, B: !Bool};CB1
type CB2 : 1S = +{A: !Int, B:?Bool};&{A: ?Int };CB2

f : CB1 -> ()
f = undefined @(CB1 -> ())

g : CB2 -> ()
g c = f c

main : () 
main = ()