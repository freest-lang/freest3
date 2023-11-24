f : Skip;Skip;Close -> Int
f x = close x; 1


main : Int
main = 
    let (x, y) = new @(Skip;Close) () in
    fork (\_:() 1-> wait y);
    f x
