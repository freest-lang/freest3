main : ()
main = fork @(!Int;Close, ?Int;Wait) (\_:() 1-> new @(!Int;Close) ())
