id' : forall a . a -> a
id' x = x

main : Int
main = id'  @Int @Bool 5
