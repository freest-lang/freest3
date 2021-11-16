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
-- type TabuadaC = (rec x: SL. &{TabuadaSimples: ?Int; x, TabuadaAte: ?Int; ?Int; x, MultiplosEntre: ?Int; ?Int; ?Int; x, Solucao: !Bool; !Int; x, Fim: Skip})

initTabuadaServer : (rec x: SL. &{TabuadaSimples: ?Int; x, TabuadaAte: ?Int; ?Int; x, MultiplosEntre: ?Int; ?Int; ?Int; x, Solucao: !Bool; !Int; x, Fim: Skip}) -o ()
initTabuadaServer c = tabuadaServer c Empty

tabuadaServer : (rec x: SL. &{TabuadaSimples: ?Int; x, TabuadaAte: ?Int; ?Int; x, MultiplosEntre: ?Int; ?Int; ?Int; x, Solucao: !Bool; !Int; x, Fim: Skip}) -o IntList -> ()
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

    Fim c ->
      ()
  }

-- Funcao de "entrada" para a "verdadeira" funcao
receiveList : (rec x: SL. +{TabuadaSimples: !Int; x, TabuadaAte: !Int; !Int; x, MultiplosEntre: !Int; !Int; !Int; x, Solucao: ?Bool; ?Int; x, Fim: Skip}) -> (IntList, (rec x: SL. +{TabuadaSimples: !Int; x, TabuadaAte: !Int; !Int; x, MultiplosEntre: !Int; !Int; !Int; x, Solucao: ?Bool; ?Int; x, Fim: Skip}))
receiveList c = receiveListAux Empty c

-- Funcao para receber uma lista pelo canal (devolve o canal no fim)
receiveListAux : IntList -> (rec x: SL. +{TabuadaSimples: !Int; x, TabuadaAte: !Int; !Int; x, MultiplosEntre: !Int; !Int; !Int; x, Solucao: ?Bool; ?Int; x, Fim: Skip}) -> (IntList, (rec x: SL. +{TabuadaSimples: !Int; x, TabuadaAte: !Int; !Int; x, MultiplosEntre: !Int; !Int; !Int; x, Solucao: ?Bool; ?Int; x, Fim: Skip}))
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
  let (r, w) = new rec x: SL. &{TabuadaSimples: ?Int; x, TabuadaAte: ?Int; ?Int; x, MultiplosEntre: ?Int; ?Int; ?Int; x, Solucao: !Bool; !Int; x, Fim: Skip} in
  let _      = fork[()] (initTabuadaServer r) in
  let c      = select TabuadaSimples w in
  let c      = send 4 c in
  let (result, c) = receiveList c in
  let c      = select Fim c in
  result
