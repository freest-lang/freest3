start :: Int
start = id' [Int] 5

id' :: forall a :: TU => a -> a
id' x = x

