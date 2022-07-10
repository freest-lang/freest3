-- I exercise 13

data Vector = Vector Int Int

produtoEscalar : Vector -> Vector -> Int
produtoEscalar a b = 
    case a of {
        Vector x1 y1 -> case b of {
                            Vector x2 y2 -> x1*x2 + y1*y2
                        }
    }

data NVector = End | NVector Int NVector

produtoEscalarNVector : NVector -> NVector -> Int
produtoEscalarNVector a b =
    case a of {
        End -> 0,
        NVector x1 rest1 ->  case b of {
                                End -> 0,
                                NVector x2 rest2 -> x1*x2 + (produtoEscalarNVector rest1 rest2)
                            }
    } 

main : Int
main = produtoEscalarNVector (NVector 1 (NVector 2 (NVector 3 End))) (NVector 1 (NVector 2 (NVector 3 End)))
-- result = 14
