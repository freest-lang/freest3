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

-- Este canal tem 3 servicos: TabuadaSimples, TabuadaAte e MultiplosEntre
-- Os resultados sao sempre armazenados e perpetuados com chamadas recursivas
--   ao proprio canal. Para obter o resultado o cliente deve fazer select
--   Solucao e ler da seguinte forma:
--     True  -> tem um Node da lista
--     False -> tem Empty e nao existe mais resultado
-- (No fundo a Solucao apenas disponibiliza o acesso ao resultado)

-- Tipo do canal (os parentesis fazem diferenca)

type TabuadaServer = &{ TabuadaSimples: ?Int ;             TabuadaServer
                           , TabuadaAte    : ?Int ; ?Int;       TabuadaServer
                           , MultiplosEntre: ?Int ; ?Int; ?Int; TabuadaServer
                           , Solucao       : !Bool; !Int;       TabuadaServer
                           , Fim           : Wait
                           }

type TabuadaClient = dualof TabuadaServer

initTabuadaServer : TabuadaServer 1-> ()
initTabuadaServer c = tabuadaServer c Empty

tabuadaServer : TabuadaServer -> IntList 1-> ()
tabuadaServer c result =
  match c with {

    -- Servicos

    TabuadaSimples  c ->
      let (x1, c) = receive c in
      let result  = tabuadaSimples x1 in
      tabuadaServer c result ,

    TabuadaAte      c ->
      let (x1, c) = receive c in
      let (x2, c) = receive c in
      let result  = tabuadaAte x1 x2 in
      tabuadaServer c result ,

    MultiplosEntre  c ->
      let (x1, c) = receive c in
      let (x2, c) = receive c in
      let (x3, c) = receive c in
      let result  = multiplosEntre x1 x2 x3 in
      tabuadaServer c result ,

    -- Solucao

    Solucao c ->
      case result of {
        Empty ->
          let c = send False c in
          let c = send 0 c in
          tabuadaServer c result,
        Node x l ->
          let c = send True c in
          let c = send x c in
          tabuadaServer c l
      },

    Fim c -> wait c
  }

-- Funcao de "entrada" para a "verdadeira" funcao
receiveList : TabuadaClient -> (IntList, TabuadaClient)
receiveList c = receiveListAux Empty c

-- Funcao para receber uma lista pelo canal (devolve o canal no fim)
receiveListAux : IntList -> TabuadaClient -> (IntList, TabuadaClient)
receiveListAux l c =
  let c      = select Solucao c in
  let (b, c) = receive c in
  let (x, c) = receive c in
  if b
  then receiveListAux (addToList x l) c
  else (l, c)

-- Funcao auxiliar para iterar a lista e adicionar um elemento no fim
addToList : Int -> IntList -> IntList
addToList i l =
  case l of {
    Empty -> Node i Empty,
    Node n l -> Node n (addToList i l)
  }


-- MAIN
main : IntList
main =
  let (r, w) = new @TabuadaServer () in
  fork @() (\_:()1-> initTabuadaServer r) ; 
  let (result, c) = select TabuadaSimples w 
                    |> send 4 
                    |> receiveList in
  select Fim c |> close;
  result
