-- V exercise 1f

addBreakLine : [Char] -> [Char]
addBreakLine list = 
    case list of {
        []        -> ('\n') :: [],
        c :: rest -> c :: (addBreakLine rest)
    }

string : [Char]
string = ['a','b','c','d','e']

main : [Char]
main = addBreakLine string
-- expected result = ['a','b','c','d','e','\n']