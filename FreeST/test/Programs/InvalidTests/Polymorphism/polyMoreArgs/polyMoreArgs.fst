

main : Int
main = id'  @Int @Bool 5

id' : forall a . a -> a
id' x = x
