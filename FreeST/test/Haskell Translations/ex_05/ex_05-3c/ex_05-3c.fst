-- V exercise 3c

data Word' = Empty | Letter Char Word'
data WordList = End | List Word' WordList

map' : (Word' -> Word') -> WordList -> WordList
map' f list = 
    case list of {
        End -> End,
        List word rest -> List (f word) (map' f rest)
    }

addS : Word' -> Word'
addS word =
    case word of {
        Empty -> Letter 's' Empty,
        Letter l rest -> Letter l (addS rest)
    }

word1 : Word'
word1 = Letter 'A' Empty
word2 : Word'
word2 = Letter 'a' (Letter 'r' (Letter 't' (Letter 'e' Empty)))
word3 : Word'
word3 = Letter 'd' (Letter 'o' Empty)
word4 : Word'
word4 = Letter 'a' (Letter 'l' (Letter 'u' (Letter 'n' (Letter 'o' Empty))))

line : WordList
line = List word1 (List word2 (List word3 (List word4 End)))

main : WordList
main = map' addS line
-- result = List (Letter 'A' (Letter 's' Empty)) (List (Letter 'a' 
-- (Letter 'r' (Letter 't' (Letter 'e' (Letter 's' Empty))))) 
-- (List (Letter 'd' (Letter 'o' (Letter 's' Empty))) (List (Letter 'a' 
-- (Letter 'l' (Letter 'u' (Letter 'n' (Letter 'o' (Letter 's' Empty)))))) End)))
