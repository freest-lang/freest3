apply : (?Int -> (Int, Skip)) -> ?Int -> (Int, Skip)
apply f = f

main : Skip
main =
    let (r, w) = new ?Int in
    fork  @(Int, Skip) (\_:() 1-> apply (receive  @Int @Skip) r);
    send 5 w
