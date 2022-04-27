-- I exercise 10

listPits : Int -> [(Int,Int,Int))]
listPits x = listPits' (1,1,1)) x

listPits' : (Int,Int,Int)) -> Int -> [(Int,Int,Int))]
listPits' t max = 
    let (a,bc) = t  in
    let (b,c)  = bc in
    if (c > max)
        then []
        else    (if (b > max)
                    then listPits' (1,1,c+1))) max
                    else    (if (a > max)
                                then listPits' (1,(b+1),c)) max
                                else    (if (a*a) + (b*b) == (c*c)
                                            then (b,a,c)) :: (listPits' ((a+1),b,c)) max)   -- list b a instead of a b for crescent purposes
                                            else listPits' ((a+1),b,c)) max
                                        )
                            )
                )

removeDups : [(Int,Int,Int))] -> [(Int,Int,Int))]
removeDups list = 
    case list of {
        [] -> [],
        t :: rest -> 
            let (a,bc) = t  in
            let (b,c)  = bc in
            (a,b,c)) :: (removeDups (removeDup (a,b) rest))
    }

removeDup : (Int,Int) -> [(Int,Int,Int))] -> [(Int,Int,Int))]
removeDup xy list = 
    let (x,y) = xy in
    case list of {
        [] -> [],
        t :: rest -> 
            let (a,bc) = t  in
            let (b,c)  = bc in
            if (x == b) && (y == a)
                then removeDup (x,y) rest
                else (a,b,c)) (removeDup (x,y) rest)
    }

main : [(Int,Int,Int))]
main = removeDups (listPits 100)
-- result = [(3, 4, 5),(6, 8, 10),(5, 12, 13),(9, 12, 15),(8, 15, 17),(12, 16, 20),(7, 24, 25),(15, 20, 25),(10, 24, 26),(20, 21, 29),(18, 24, 30),(16, 30, 34),(21, 28, 35),(12, 35, 37),(15, 36, 39),(24, 32, 40),(9, 40, 41),(27, 36, 45),(14, 48, 50),(30, 40, 50),(24, 45, 51),(20, 48, 52),(28, 45, 53),(33, 44, 55),(40, 42, 58),(36, 48, 60),(11, 60, 61),(16, 63, 65),(25, 60, 65),(33, 56, 65),(39, 52, 65),(32, 60, 68),(42, 56, 70),(48, 55, 73),(24, 70, 74),(21, 72, 75),(45, 60, 75),(30, 72, 78),(48, 64, 80),(18, 80, 82),(13, 84, 85),(36, 77, 85),(40, 75, 85),(51, 68, 85),(60, 63, 87),(39, 80, 89),(54, 72, 90),(35, 84, 91),(57, 76, 95),(65, 72, 97),(28, 96, 100),(60, 80, 100)]