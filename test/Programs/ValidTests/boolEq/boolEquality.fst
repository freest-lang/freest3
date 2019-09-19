

main : Bool
main = boolEq True False

boolEq : Bool -> Bool -> Bool
boolEq b1 b2 = if (b1 && b2) || (not b1 && not b2) then True else False
