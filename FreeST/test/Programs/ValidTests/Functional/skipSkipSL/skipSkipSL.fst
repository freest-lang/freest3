f : Skip;Skip;EndC -> Int
f x = close x; 1

main : Int
main = 
    let (c, s) = new @(Skip;Skip;EndC) () in
    fork (\_:() 1-> wait s);
    f c
