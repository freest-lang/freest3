-- V exercise 3d

data Word' = Empty | Letter Char Word'
data WordList = End | List Word' WordList

map' : (Word' -> Word') -> WordList -> WordList
map' f list = 
    case list of {
        End -> End,
        List word rest -> List (f word) (map' f rest)
    }

addS : Word' -> Word'
addS word = Letter 's' word

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
-- result = List (Letter 's' (Letter 'A' Empty)) (List 
-- (Letter 's' (Letter 'a' (Letter 'r' (Letter 't' 
-- (Letter 'e' Empty))))) (List (Letter 's' (Letter 'd' 
-- (Letter 'o' Empty))) (List (Letter 's' (Letter 'a' 
-- (Letter 'l' (Letter 'u' (Letter 'n' 
-- (Letter 'o' Empty)))))) End)))