main : ()
main = 
    let (x, y) = new @Close () in
    close x;
    close x;
    wait y
