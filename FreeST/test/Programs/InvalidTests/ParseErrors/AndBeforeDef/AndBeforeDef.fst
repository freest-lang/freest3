and 
even n | n == 0    = True 
       | n  > 0    = odd (n - 1)
       | otherwise = odd (n + 1)

and
odd n | n == 0    = False 
      | n  > 0    = even (n - 1)
      | otherwise = even (n + 1)