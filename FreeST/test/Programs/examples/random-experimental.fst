type Bit = Int

unbiasedBit : *?Bit -> Bit
unbiasedBit c =
  let (b1, c) = receive c in
  let (b2, c) = receive c in
  -- printInt b1;
  -- printInt b2;
  if b1 == b2 then unbiasedBit c else b1

type Diverge = Char

produce : Bit -> *!Bit -> Diverge
produce b c = produce (1 - b) $ send b c

collect' : *?Bit -> Int -> Int
collect' c n =
  if n == 0
  then 0
  else unbiasedBit c + 2 * collect' c (n - 1)

bits' : Int -> Int
bits' =
  let (r, w) = new @*?Bit () in
  fork (\_:() 1-> produce 1 w);
  fork (\_:() 1-> produce 0 w);
  collect' r

----

biasedBit : Bit
biasedBit =
  let (r, w) = new @*?Int () in --new @*?Int () in
  fork (\_:() 1-> send 0 w);
  fork (\_:() 1-> send 1 w);
  let (b, _) = receive r in
  b

unbiasedBit' : Bit
unbiasedBit' =
  let b = biasedBit in
  if b == biasedBit then unbiasedBit' else b
  
bits : Int -> Int
bits n =
  if n == 0
  then 0
  else unbiasedBit' + 2 * bits (n - 1)

-----------------------------------------------------------------------

-- sorted binary tree with (value, frequency)
data DistribuitionTree = Node (Int, Int) DistribuitionTree DistribuitionTree | Leaf

isLeaf : DistribuitionTree -> Bool
isLeaf dTree =
    case dTree of {
        Node _ _ _ -> False,
        Leaf       -> True
    }

insert : Int -> DistribuitionTree -> DistribuitionTree
insert value dTree =
    case dTree of {
        Node pair left right -> 
            let (value', frequency') = pair in
            if value == value'
            then 
                Node (value', frequency'+1) left right
            else 
                if value' < value
                then Node pair left (insert value right)
                else Node pair (insert value left) right,
        Leaf -> 
            Node (value, 1) Leaf Leaf
    }

treeMaxFreq : Int -> DistribuitionTree -> Int
treeMaxFreq default dTree =
    case dTree of {
        Node pair left right ->
            let (_, freq) = pair in
            let maxFreqL  = treeMaxFreq default left  in
            let maxFreqR  = treeMaxFreq default right in
            --
            if freq > maxFreqL
            then
                if freq > maxFreqR
                then freq
                else maxFreqR
            else
                if maxFreqL > maxFreqR
                then maxFreqL
                else maxFreqR,
        Leaf -> 
            default
    }

calcRatio : Int -> Int -> Int -> Int
calcRatio r maxBars maxFreq =
    if maxBars > maxFreq / r
    then r
    else calcRatio (r+1) maxBars maxFreq

printTree : DistribuitionTree -> ()
printTree dTree =
    let maxBars = 100 in
    let maxFreq = treeMaxFreq 0 dTree in
    let ratio   = calcRatio 1 maxBars maxFreq in
    printTree' ratio dTree

printTree' : Int -> DistribuitionTree -> ()
printTree' ratio dTree =
    case dTree of {
        Node pair left right ->
            printTree' ratio left;
            printFreq  ratio pair;
            printTree' ratio right,
        Leaf ->
            printString ""
    }

printFreq : Int -> (Int, Int) -> ()
printFreq ratio pair = 
    let (value, frequency) = pair in
    printInt value; printString "\t("; printInt frequency; printString ")\t";
    repeatF (frequency / ratio) (\_:() -> printString "x"); printStringLn ""

repeatF : Int -> (() -> ()) -> ()
repeatF n f =
    if n > 0
    then f (); repeatF (n-1) f
    else ()

testDistribuition : Int -> (() -> Int) -> DistribuitionTree
testDistribuition n f =
    if n < 1
    then Leaf
    else insert (f ()) $ testDistribuition (n-1) f 

testLongestRun : Int -> (() -> Int) -> DistribuitionTree
testLongestRun size f =
    let i = f () in
    testLongestRun' i 1 (size-1) f

testLongestRun' : Int -> Int -> Int -> (() -> Int) -> DistribuitionTree 
testLongestRun' initial count size f =
    if size < 1
    then 
        Node (count, 1) Leaf Leaf
    else 
        let i = f () in
        if i == initial
        then testLongestRun' initial (count+1) (size-1) f
        else insert count $ testLongestRun' i 1 (size-1) f

runTestSuite : Int -> (() -> Int) -> ()
runTestSuite n f =
    printStringLn "===== DISTRIBUITION =====";
    printTree $ testDistribuition n f;
    printStringLn ""
    -- printStringLn "=====  LONGEST RUN  =====";
    -- printTree $ testLongestRun n f;
    -- printStringLn ""

------------------------------------------------------------------------------------

type NoiseC : SU = !Int; NoiseC

noiseServer : NoiseC -> ()
noiseServer c = noiseServer $ send 0 c

noiseClient : dualof NoiseC -> ()
noiseClient c = noiseClient $ snd[Int, dualof NoiseC] $ receive c

main : ()
main =
    -- some noise, uncommenting these lines affects randomness 
    -- let (s, c) = new @NoiseC () in
    -- fork (\_:() 1-> noiseServer s);
    -- fork (\_:() 1-> noiseClient c);
    --
    let n = 1000 in
    let f2 = (\_:() -> bits 5) in
    runTestSuite n f2
    
