-- I exercise 12

getPows : Int -> Int -> [Int]
getPows p n = if n == 0 
                then []
                else (exp' p n) :: (getPows p (n-1))

exp' : Int -> Int -> Int
exp' p n = if n == 0
            then 1
            else p * (exp' p (n-1))

main : [Int]
main = getPows 2 50
-- 1125899906842624 :: 562949953421312 :: 281474976710656 :: 140737488355328 :: 70368744177664 :: 35184372088832 :: 17592186044416 :: 8796093022208 :: 4398046511104 :: 2199023255552 :: 1099511627776 :: 549755813888 :: 274877906944 :: 137438953472 :: 68719476736 :: 34359738368 :: 17179869184 :: 8589934592 :: 4294967296 :: 2147483648 :: 1073741824 :: 536870912 :: 268435456 :: 134217728 :: 67108864 :: 33554432 :: 16777216 :: 8388608 :: 4194304 :: 2097152 :: 1048576 :: 524288 :: 262144 :: 131072 :: 65536 :: 32768 :: 16384 :: 8192 :: 4096 :: 2048 :: 1024 :: 512 :: 256 :: 128 :: 64 :: 32 :: 16 :: 8 :: 4 :: 2 :: []