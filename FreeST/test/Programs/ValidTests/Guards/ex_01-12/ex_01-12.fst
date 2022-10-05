-- I exercise 12

data IntList = Nil | List Int IntList

getPows : Int -> Int -> IntList
getPows p n 
  | n == 0    = Nil
  | otherwise = List (exp' p n) (getPows p (n-1))

exp' : Int -> Int -> Int
exp' p n 
  | n == 0    = 1
  | otherwise = p * (exp' p (n-1))

main : IntList
main = getPows 2 50
-- List 1125899906842624 (List 562949953421312 (List 281474976710656 (List 140737488355328 (List 70368744177664 
-- (List 35184372088832 (List 17592186044416 (List 8796093022208 (List 4398046511104 (List 2199023255552 
-- (List 1099511627776 (List 549755813888 (List 274877906944 (List 137438953472 (List 68719476736 (List 34359738368 
-- (List 17179869184 (List 8589934592 (List 4294967296 (List 2147483648 (List 1073741824 (List 536870912 
-- (List 268435456 (List 134217728 (List 67108864 (List 33554432 (List 16777216 (List 8388608 (List 4194304 
-- (List 2097152 (List 1048576 (List 524288 (List 262144 (List 131072 (List 65536 (List 32768 (List 16384 
-- (List 8192 (List 4096 (List 2048 (List 1024 (List 512 (List 256 (List 128 (List 64 (List 32 (List 16 
-- (List 8 (List 4 (List 2 Nil)))))))))))))))))))))))))))))))))))))))))))))))))
