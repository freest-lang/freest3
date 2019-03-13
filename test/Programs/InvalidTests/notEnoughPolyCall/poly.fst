id' : forall x => x -> x
id' c = c

start : Int
start =
  id' 5
