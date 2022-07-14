
sink : Skip -> ()
sink c = ()

data Data = A | B
type Channel : 1S = &{C: Skip,D: Skip}

f : Data -> Channel -> Int
f x c = 
  match c with {
    C c -> 
      case x of {
        A -> 1,
        B -> 2
      },
    D c -> 
      case x of {
        A -> 1,
        B -> 2
      }
  }

g : Channel -> Data 1-> Int
g c x = 
  match c with {
    C c -> 
      case x of {
        A -> 1,
        B -> 2
      },
    D c -> 
      case x of {
        A -> 1,
        B -> 2
      }
  }

main : ()
main =
  let (w,r) = new Channel in
  select C r;
  printIntLn $ f A w
  ;
  let (w,r) = new Channel in
  select C r;
  printIntLn $ g w A
