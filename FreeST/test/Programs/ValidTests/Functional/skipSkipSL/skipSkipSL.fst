f : Skip;Skip;End -> Int
f x = close x; 1

main : Int
main = 
    let (c, s) = new @(Skip;Skip;End) () in
    fork (\_:() 1-> close s);
    f c
