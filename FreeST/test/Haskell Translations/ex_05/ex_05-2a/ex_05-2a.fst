-- V exercise 2a

data IntTuple = IT Int Int

add1 : IntTuple -> Int
add1 tuple =
    case tuple of {
        IT x y -> x + y
    }

main : Int
main = add1 (IT 10 20)
-- result = 30
