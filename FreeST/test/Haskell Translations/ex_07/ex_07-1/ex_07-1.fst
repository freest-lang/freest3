-- VII exercise 1

data GeoForm = Circle Int | Triangle Int Int Int | Rectangle Int Int Int Int
--                   |radius        |sides                  |sides

perimeter : GeoForm -> Int
perimeter form =
    case form of {
        Circle r -> r*3,        -- there's no pi so we round it to 3
        Triangle x y z -> x+y+z,
        Rectangle w x y z -> w+x+y+z
    }

isRegular : GeoForm -> Bool
isRegular form =
    case form of {
        Circle _ -> True,
        Triangle x y z -> (x == y) && (y == z),
        Rectangle w x y z -> (w == x) && (x == y) && (y == z)
    }

main : Bool 
main = 
    let circle = Circle 10 in
    let triangle1 = Triangle 10 10 10 in
    let triangle2 = Triangle 15 15 10 in
    let rectangle = Rectangle 10 10 10 10 in
    (perimeter circle) == (perimeter triangle1) && 
    (perimeter triangle2) == (perimeter rectangle) &&
    (isRegular circle) && (isRegular triangle1) &&
    (not (isRegular triangle2)) && (isRegular rectangle)
--result = True
