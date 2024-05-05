type CB1 = +{A: !Int};&{A: ?Int, B: !Bool};CB1
type CB2 = +{A: !Int, B:?Bool};&{A: ?Int };CB2

f : CB1 -> ()
f = undefined @(CB1 -> ())

g : CB2 -> ()
g c = f c

main : () 
main = ()