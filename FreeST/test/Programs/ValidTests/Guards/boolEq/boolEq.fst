

main : Bool
main = boolEq True False

boolEq : Bool -> Bool -> Bool
boolEq b1 b2 
  | (b1 && b2) || (not b1 && not b2) = True 
  | otherwise                        = False
