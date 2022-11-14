main : ()
main = 
    let (x, y) = new @End () in
    close x;
    close x;
    close y;
