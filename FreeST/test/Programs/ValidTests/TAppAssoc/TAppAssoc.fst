send' : ∀a:ML . a -> ∀b:SL . !a;b -o b
send' x c = send x c

g : !Int -> Skip
g c =  send' [Int] 5 [Skip] c

main : Skip
main = 
  let (x, y) = new !Int in
  let _ = fork[(Int, Skip)] (receive y) in
  g x
   
