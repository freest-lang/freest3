-- I exercise 12

data Value = T | F | U

-- without pattern matching
disjunction : Value -> Value -> Value
disjunction a b = 
    case a of {
        T -> T,
        F -> case b of {
                T -> T,
                F -> F,
                U -> F
            },
        U -> case b of {
                U -> U,
                F -> F,
                T -> T
            }
    }

-- with pattern matching
-- (\/) :: Value -> Value -> Value
-- (\/) T _ = T
-- (\/) _ T = T
-- (\/) U U = U
-- (\/) _ _ = F

main : Value
main = disjunction U F
-- result = F