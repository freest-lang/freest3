id' : a -> a 
id' x = x 

const' : a -> ∀b:*T . b -> a 
const' x y = x 

main : ()
main = id' @() (const' @() () @Int 0) 