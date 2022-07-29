
matrix :: [([Int],Int)]
matrix = [ ([1,2,3,4,5],1)
         , ([1,2,3,4,5],2)
         , ([1,2,3,4,5],3)
         ]

f = map (g . map fst)

g = (++) [[1]]

main = putStrLn $ show $ f [matrix,matrix,matrix]