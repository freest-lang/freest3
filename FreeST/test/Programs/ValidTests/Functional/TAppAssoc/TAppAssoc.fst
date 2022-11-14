g : !Int -> Skip
g c =  (send  @Int 5)  @Skip c

main : Skip
main = 
  let (x, y) = new @!Int  () in
  let _ = fork @(Int, Skip) \_:() 1-> (receive y) in
  g x
   
