-- ==================== Estruturas ====================

data IntList = Empty | Node Int IntList


-- ==================== Servicos ====================

tabuadaSimples : Int -> IntList
tabuadaSimples i = tabuadaAte i 10

tabuadaAte : Int -> Int -> IntList
tabuadaAte i n = multiplosEntre i 1 n

multiplosEntre : Int -> Int -> Int -> IntList
multiplosEntre i x n = if x == n
                       then Node (i * x) Empty
                       else Node (i * x) (multiplosEntre i (x + 1) n)

-- ==================== Servidor ====================

type TabuadaC : 1S = +{ TabuadaSimples: !Int; TabuadaC
                      , TabuadaAte: !Int; !Int; TabuadaC
                      , MultiplosEntre: !Int; !Int; !Int; TabuadaC
                      , Solucao: ?Bool; ?Int; TabuadaC
                      , Fim: End
                      }

-- Este canal tem 3 servicos: TabuadaSimples, TabuadaAte e MultiplosEntre
-- Os resultados sao sempre armazenados e perpetuados com chamadas recursivas
--   ao proprio canal. Para obter o resultado o cliente deve fazer select
--   Solucao e ler da seguinte forma:
--     True  -> tem um Node da lista
--     False -> tem Empty e nao existe mais resultado
-- (No fundo a Solucao apenas disponibiliza o acesso ao resultado)

initTabuadaServer : dualof TabuadaC -> ()
initTabuadaServer c = tabuadaServer c Empty

tabuadaServer : dualof TabuadaC -> IntList 1-> ()
tabuadaServer (Fim            c) result = close c
-- Servicos
tabuadaServer (TabuadaSimples c) result =
  let (x1, c) = receive c in
  let result  = tabuadaSimples x1 in
  tabuadaServer c result
tabuadaServer (TabuadaAte     c) result =
  let (x1, c) = receive c in
  let (x2, c) = receive c in
  let result  = tabuadaAte x1 x2 in
  tabuadaServer c result
tabuadaServer (MultiplosEntre c) result =
  let (x1, c) = receive c in
  let (x2, c) = receive c in
  let (x3, c) = receive c in
  let result  = multiplosEntre x1 x2 x3 in
  tabuadaServer c result
-- Solucao
tabuadaServer (Solucao        c) Empty =
  let c = send False c in
  let c = send 0 c in
  tabuadaServer c Empty
tabuadaServer (Solucao        c) (Node x l) =
  let c = send True c in
  let c = send x c in
  tabuadaServer c l
--

-- Funcao de "entrada" para a "verdadeira" funcao
receiveList : TabuadaC -> (IntList, TabuadaC)
receiveList c = receiveListAux Empty c

-- Funcao para receber uma lista pelo canal (devolve o canal no fim)
receiveListAux : IntList -> TabuadaC -> (IntList, TabuadaC)
receiveListAux l c =
  let c      = select Solucao c in
  let (b, c) = receive c in
  let (x, c) = receive c in
  if b
  then receiveListAux (addToList x l) c
  else (l, c)

-- Funcao auxiliar para iterar a lista e adicionar um elemento no fim
addToList : Int -> IntList -> IntList
addToList i Empty      = Node i Empty
addToList i (Node n l) = Node n (addToList i l)

-- MAIN
main : IntList
main =
  let (c, s) = new @TabuadaC () in
  fork (\_:() 1-> initTabuadaServer s) ;
  let c      = select TabuadaSimples c in
  let c      = send 4 c in
  let (result, c) = receiveList c in
  let c      = select Fim c in
  close c;
  result
