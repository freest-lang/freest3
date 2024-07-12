receiveType : forall a:1S . forall b:1S . a -> (TypeVar, b)
sendType : TypeVar -> forall a:1S . a -> forall b:1S . b

type HPServer : 1S = forall a:*T . ?(() 1-> a) ; !a ; Skip

server : HPServer -> ()
server c0 = 
  let (a, c1) = receiveType c0 in
  let (comp, c2) = receive c1 in
  fork (\_:() 1-> (send comp c2) |> close)