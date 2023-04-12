f : Skip;Skip;End -> Int
f x = close x; 1


main : Int
main = 
    let (x, y) = new @(Skip;End) () in
    fork (\_:() 1-> close y);
    f x
