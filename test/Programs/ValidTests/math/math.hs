
mathServer :: &{Opp: ?Int;!Int;Skip, Plus: ?Int;?Int;!Int;Skip} -> Skip
mathServer c =
  case c of
    Opp x ->
      let n, c1 = receive x in
      send (-n) c1
      
    Plus y ->
      let n1, c1 = receive y in
      let n2, c2 = receive c1 in
      send (n1+n2) c2  
        
          
start :: ()
start = ()
 
        
