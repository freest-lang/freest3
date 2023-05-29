f : Skip;Skip;EndC -> Int
f x = close x; 1


main : Int
main = 
    let (x, y) = new @(Skip;EndC) () in
    fork (\_:() 1-> wait y);
    f x
