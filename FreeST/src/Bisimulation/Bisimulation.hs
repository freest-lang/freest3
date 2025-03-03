{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase, BlockArguments, NamedFieldPuns, ViewPatterns #-}
module Bisimulation.Bisimulation 
 ( bisimilar
 ) where

import Bisimulation.Norm
import Bisimulation.State
import Bisimulation.Congruence 
import SimpleGrammar.Grammar
import Syntax.Base
import Prelude hiding (Word, log)

import Control.Monad.State (State, evalState, gets, modify, runState, foldM)
import Data.Foldable (foldrM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- | Are two words over a simple grammar bisimilar?
bisimilar :: Grammar -> Bool
bisimilar g@(Grammar [γ, δ] _) =                                             -- IMO the grammar should only include the productions
  evalState (basisUpdate [] Set.empty initQueue) initState                   -- (with γ and δ as parameters to bisimilar)
  where
    initQueue = Seq.singleton Node{pair = (γ, δ), parent = Nothing}
    initState = BisimulationState
      { basis = initBasis g
      , visitedPairs = Set.empty
      , normMap = Map.empty
      , grammar = g
      , log = []
      }
    initBasis g =
      Set.foldr (\α b -> Map.insert (α, α) (Bpa1 []) b) Map.empty (nonterminals g)

-- | The basis-updating algorithm for deciding the bisimilarity of two words
-- over a simple grammar. The main ideas driving the algorithm are as follows:
-- 
--   * The algorithm works by building a derivation tree, whose nodes are pairs
--     of words \((\gamma, \delta)\). Such a node intuitively corresponds to 
--     the goal of determining whether \(\gamma \sim \delta\)@.
--   * The algorithm keeps track of a basis 
--       \(\mathcal B \subseteq \mathcal V^{+} \times \mathcal V^{+}\)
--     and a set 
--       \(\mathcal S \subseteq \mathcal V \times \mathcal V\) 
--     of pairs of nonterminals.
--   * Initially, the tree has the single leaf node \((\gamma, \delta)\) 
--     corresponding to the pair of words given as input; \(\mathcal B\) is 
--     comprised of the pairs \((X, X)\) for every nonterminal \(X\); and 
--     \(\mathcal S = \emptyset\).
--   * \(\mathcal B\) and \(\mathcal S\) may be updated in the following ways:
--
--        * adding a pair \(X, Y \beta\) to \(\mathcal B\), where \((X, Y)\) is
--          not in \(\mathcal S\);
--        * removing a pair \((X, Y \beta)\) from \(\mathcal B\), where 
--          \((X, Y)\) is not in \(\mathcal S\) and may be added to it;
--        * replacing a pair \((X, Y \beta)\) in \(\mathcal B\) by a pair
--          \((X \alpha, Y \beta')\), where \((X, Y)\) is not in \(\mathcal S\)
--          and is added to \(\mathcal S\);
--        * removing a pair \((X \alpha, Y \beta)\) from \(\mathcal B\), where
--          \((X, Y)\) is in \(\mathcal S\).
--
--   * Each internal node in the derivation tree is either unmarked, marked as 
--     a BPA1 guess, or marked as a BPA2 guess. Each leaf in the derivation
--     tree is either finished or unfinished. On each iteration (expansion 
--     step), the algorithm chooses the first (in a depth-first search) 
--     unfinished leaf to be expanded, which may result in four possible
--     outcomes:
--
--        * zero children (the leaf becomes finished)
--        * one or more children (all of which become unfinished leaves)
--        * partial failure (a portion of the tree is pruned and the basis is
--          updated)
--        * total failure (the algorithm terminates concluding that the initial
--          pair is not bisimilar)
-- 
--   * If all leaves are finished, the algorithm terminates concluding that the
--     initial pair is bisimilar.
--
-- Expansion steps that are not failures may add a pair to \(\mathcal B\) or 
-- leave it unchanged. In the latter case, they correspond to applications of 
-- coinductive congruence rules. The algorithm shall preserve as an invariant
-- the property that \(\mathcal B\) is reflexive, norm-compliant, functional
-- and simple. This will ensure that expansion steps which leave \(\mathcal B\)
-- unchanged cannot go on forever.
-- 
-- We shall assume a fixed ordering of the nonterminals 
--   \(\mathcal V = \{X^{(0)} \lt X^{(1)} \lt \ldots \lt X^{(|V|)}\}\) 
-- such that 
--   \(\|X\| \lt \|Y\|\) implies \(X \lt Y\). 
-- We shall also assume that, whenever we add a new pair \((\gamma, \delta)\) 
-- to the derivation tree, if \(\gamma\) is unnormed then it is of the form 
--   \(\gamma = \alpha X\) with \(\alpha\) normed and \(X\) unnormed;
-- and similarly for \(\delta\). In other words, all words considered 
-- throughout the algorithm abide by the pruning convention. This convention 
-- can be enforced by removing all nonterminals after the first unnormed 
-- nonterminal, according to the pruning lemma.
basisUpdate :: [(Variable, Variable)]                                          -- the name of this parameter 'track' is not very elucidating. rename to what?
            -> Set.Set (Variable, Variable) 
            -> BranchQueue 
            -> Bisimulation Bool
basisUpdate track s = \case
  (Seq.viewl -> Seq.EmptyL) -> return True
  q@(Seq.viewl -> node'@Node{parent} Seq.:< q') -> do                          -- proposal: @type Node = NonEmpty (Word, Word)@
    b <- gets basis                                                            -- ('Traversable' etc. instances would help with node transformations.)
    g <- gets grammar
    node@Node{pair = (γ, δ)} <- case parent of
        Nothing -> pruneNode node' >>= orderNode
        _       -> return node'
    visited <- gets visitedPairs
    -- We are now ready to describe the possible ways in which a leaf can be
    -- expanded. Suppose that the algorithm examines an unfinished leaf
    -- \((\gamma, \delta)\). Each of the following cases is considered in order.
    if  -- __Case 1__ (Loop detection).
        -- if \((\gamma, \delta)\) coincides with an already visited node
        -- (either an internal node, or some finished leaf in the current tree),
        -- then the expansion of this leaf produces zero children and the leaf
        -- becomes finished. This corresponds to detecting a loop in the 
        -- coinductive congruence algorithm.
        (γ, δ) `elem` visited || (γ, δ) `elem` visited
        -- __Case 2__ (Identical words).
        -- If \(\gamma = \delta\), then the expansion of this leaf produces zero
        -- children and marks the leaf as finished. This corresponds to
        -- successively applying rule BPA1 with pairs of identical nonterminals
        -- followed by rule ε-Ax. Note that this also includes the case in which
        -- \(\gamma\) and \(\delta\) are both empty.
        || γ == δ
    then basisUpdate track s q'
    else do
      modifyVisitedPairs (Set.insert (γ, δ))
      nγ <- normUsingMap Set.empty γ
      nδ <- normUsingMap Set.empty δ
      if -- __Case 3__ (Empty vs. nonempty).
          -- If \(\gamma =    \varepsilon\) and \(\delta \neq \varepsilon\),
          -- or \(\gamma \neq \varepsilon\) and \(\delta \eq  \varepsilon\),
          -- then the expansion of this leaf is a partial failure (described in
          -- __Case 10__).
          nγ /= nδ
      then partialFail node track q' s
      else do
        -- From now on, we can assume that \(\gamma\) and \(\delta\) are both
        -- non-empty. Let us rewrite 
        --   \(\gamma = X \alpha'\) and \(\delta = Y \beta'\).
        let (x : α', y : β') = (γ, δ)
        -- We assume \(X \ge Y\), the symmetric cases being handled similarly.
        -- The next cases will consider whether \(\mathcal B\) already contains
        -- a pair associated with \(X\), \(Y\).
        case Map.lookup (x, y) b of
          -- __Case 4__ (Basis includes pair, BPA1 expansion).
          -- If \(\mathcal B\) contains a pair \((X, Y \beta)\), the expansion
          -- of this leaf produces a single child \((\beta \alpha', \beta')\). 
          -- This corresponds to applying rule BPA1 to \((X \alpha', Y \beta')\).
          -- __Case 5__ (Basis includes pair, BPA2 expansion).
          -- If \(\mathcal B\) contains a pair 
          --   \((X \alpha, Y \beta)\) with \(\alpha \neq \varepsilon\),
          -- the expansion of this leaf produces two children:
          --   \((\alpha, \alpha')\) and 
          --   \((\beta , \beta' )\). 
          -- This corresponds to applying rule BPA2 to \((X \alpha', Y \beta')\).
          Just _ -> do
            children <- mapM orderPairByNorm (applyRules b (γ, δ))
            q''      <- foldM (enqueuePrunedNode node) q' children
            basisUpdate track s q''
          -- For the remaining cases, we assume that \(\mathcal B\) does not contain
          -- a pair associated with \(X\), \(Y\).
          Nothing
            -- __Case 6__ (Basis does not include pair, transitions do not
            -- match, total failure).
            -- If the transitions of \(X\) and \(Y\) do not match, i.e., there
            -- is some \(X \xrightarrow a \gamma'\) without a corresponding 
            --         \(Y \xrightarrow a \delta'\) 
            -- or vice-versa, then the expansion of this leaf is a total 
            -- failure. The algorithm terminates concluding that the initial
            -- pair is not bisimilar.
            | not (transitionsMatch g x y) -> return False
            -- From now on, we assume that all transitions of \(X\) and \(Y\) 
            -- match.
            | otherwise -> case (norm g x, norm g y) of
              -- __Case 7__ (Basis does not include pair, transitions 
              -- match, both unnormed). Suppose that \(X\) and \(Y\) are both
              -- unnormed. By the pruning convention, we may assume 
              -- that \(\alpha' = \beta' = \varepsilon\).
              -- 
              --   1. Update \(\mathcal B\) by adding the BPA1 pair \((X, Y)\).
              --   2. In the derivation tree, mark \((X, Y)\) as a BPA1 guess.
              --   3. For each pair of matching transitions
              --        \(X \xrightarrow{a_i} \gamma_i\), 
              --        \(Y \xrightarrow{a_i} \delta_i\),
              --      add the node \((\gamma_i, \delta_i)\) as a child of
              --        \((X, Y)\).
              (Unnormed, Unnormed) -> addMatchingTransitions [] node q'
              -- __Case 8__ (Basis does not include pair, transitions match, 
              -- unnormed vs. normed).
              -- Suppose that \(X\) is unnormed but \(Y\) is normed. By the pruning
              -- convention, we may assume that \(\alpha' = \varepsilon\).
              (Unnormed, Normed _) -- We consider two subcases:
                -- If \(Y \beta'\) is unnormed, then:
                --
                --   1. Update \(\mathcal B\) by adding the BPA1 pair 
                --      \((X, Y \beta')\).
                --   2. In the derivation tree, mark node \((X, Y \beta')\) as
                --      a BPA1 guess.
                --   3. For each pair of matching transitions 
                --        \(X \xrightarrow{a_i} \gamma_i\),
                --        \(Y \xrightarrow{a_i} \delta_i\),
                --      add the (pruning of) node 
                --        \((\gamma_i, \delta_i \delta')\)
                --      as a child of \((X, Y \beta')\).
                | not (isNormed g δ) -> addMatchingTransitions β' node q'
                -- If \(Y \beta'\) is normed, then execute the partial failure
                -- routine (__Case 10__) on node \((X, Y \beta')\).
                | otherwise -> partialFail2 node track q s                     -- Does this deviate from the paper?
                -- The symmetric case in which \(X\) is normed and \(Y\) is
                -- unnormed is handled similarly.
              -- __Case 9__ (Basis does not include pair, transitions match,
              -- both normed).
              -- Finally, suppose that \(X\) and \(Y\) are both normed, with 
              -- \(\|X\| \ge \|Y\|\). Let @Y \xrightarrow u \varepsilon\) be
              -- the canonical norm-reducing sequence and let \(\beta\) be the
              -- \(\|Y\|\)th term in the canonical seminorm-reducing sequence
              -- of \(X\).
              (Normed _, Normed _) -- We consider the following subcases:
                -- \((X, Y)\) is not in \(\mathcal S\):
                | (x, y) `notElem` s ->
                  computeβ x y >>= \case
                  -- If \(X \xrightarrow u \beta\), then:
                  -- 
                  --   1. Update \(\mathcal B\) by adding the pair 
                  --        \((X, Y \beta)\).
                  --   2. In the derivation tree, mark node 
                  --        \((X \gamma', Y \delta')\)
                  --      as a BPA1 guess.
                  --   3. For each pair of matching transitions 
                  --        \(X \xrightarrow {a_i} \gamma_i\),
                  --        \(Y \xrightarrow {a_i} \delta_i\),
                  --      add the (pruning of) node
                  --        \((\gamma_i, \delta_i \beta)\)
                  --      as a child of \((X \alpha', Y \beta')\).
                  Just β -> do
                    q'' <- getChildren node >>= addβToPair β g >>= mapM orderNode
                    let b' = Map.insert (x, y) (Bpa1 β) b
                    putBasis b'
                    mapM orderPairByNorm (applyRules b' (γ, δ))
                      >>= foldM (enqueuePrunedNode node) (q'' Seq.>< q')
                      >>= basisUpdate ((x, y) : track) s
                    where
                      addβToPair x g = traverse (pruneNode . addToSecond x)
                      
                      addToSecond zs Node{pair = (xs, ys), parent} =
                        Node{ pair  = (xs, ys ++ zs)
                            , parent = addToSecond zs <$> parent}
                  -- Otherwise,...                                             -- TODO: can we join this case with the one below and avoid defining 'addPairBpa2ToBasis'?
                  Nothing -> do
                    let s' = Set.insert (x, y) s
                    case (nγ, nδ) of
                      (Unnormed, Unnormed) -> do
                        addPairBpa2ToBasis node q' (α', β') track s'
                      _ -> partialFail2 node track q s'
                -- If \((X, Y) \in \mathcal S\), we consider the following
                -- subcases:
                | otherwise -> case (nγ, nδ) of
                  -- If @x : α'@ and @y : β'@ are both unnormed, then:
                  -- 
                  --   1. Update \(\mathcal B\) by adding the pair 
                  --        \((X \alpha', Y \beta')\).
                  --   2. Update \(\mathcal S\) by adding the pair \((X, Y)\). 
                  --      (if it is still not in \(\mathcal S\)).
                  --   3. In the derivation tree, mark node 
                  --        \((X \alpha', Y \beta')\) 
                  --      as a BPA2 guess.
                  --   4. For each pair of matching transitions 
                  --        \(X \xrightarrow {a_i} \gamma_i\), 
                  --        \(Y \xrightarrow {a_i} \delta_i\),
                  --      add the (pruning of) node 
                  --        \((\gamma_i \alpha', \delta_i \beta′)\) 
                  --      as a child of \((X \alpha', Y \beta')\).
                  (Unnormed, Unnormed) ->
                    addPairBpa2ToBasis node q' (α', β') track s
                  -- If \((X, Y)\) is in \(\mathcal S\) or 
                  --   \(X \not \xrightarrow u \beta\), 
                  -- and one of \(X \gamma'\), \(Y \delta'\) is normed, then:
                  --   1. Update \(\mathcal S\) by adding the pair \((X, Y)\)
                  --      (if it is still not in \(S\).
                  --   2. Execute the partial failure routine (__Case 10__) on 
                  --      node \((X \gamma', Y \delta')\).
                  _ -> partialFail2 node track q s
  where
    -- __Case 10__ (Partial failure)
    -- In a partial failure, the algorithm moves up the tree, removing some 
    -- of the nodes and updating the basis.
    partialFail node track q s = case parent node of
      -- When executing a partial failure on a given node \((\gamma, \delta)\), the
      -- algorithm considers the following subcases:
      --
      -- 1. If \((\gamma, \delta)\) is the root node of the tree, then the partial failure
      -- becomes a total failure. The algorithm terminates concluding that
      -- the words in the initial pair are not bisimilar.
      Nothing -> return False
      -- Otherwise, if \((\gamma, \delta)\) is not the root node, then it has a parent, 
      -- call it \((X \alpha, Y \beta)\). We assume \(X \ge Y\), the symmetric cases 
      -- being handled similarly.
      Just node'@(Node (γ@(x : α), δ@(y : β)) _)-> do
        gets (Map.lookup (x, y) . basis) >>= \case 
          -- If \((X \alpha, Y \beta)\) is a BPA1 guess, \((\gamma, \delta)\)
          -- was obtained from \((X \alpha, Y \beta)\) by a pair of matching 
          -- transitions.
          Just (Bpa1 _) -> do
            nγ <- normUsingMap Set.empty γ
            nδ <- normUsingMap Set.empty δ
            case (nγ, nδ) of
              -- 2. If and \(X \alpha\), \(Y \beta\) are both unnormed, then:
              --
              --   1. Update \(\mathcal B\) by replacing the pair associated
              --      with \(X\), \(Y\) by the pair \((X \alpha, Y \beta)\).
              --   2. Update \(\mathcal S\) by adding the pair \((X, Y)\).
              --   3. In the derivation tree, mark node \((X \alpha, Y \beta)\)
              --      as a BPA2 guess.
              --   4. Prune the tree by removing every node below 
              --      \((X \alpha, Y \beta)\). This includes 
              --        \((\gamma, \delta)\) and its descendants,
              --        the sibling nodes of \((\gamma, \delta)\),
              --        and their descendants.
              --      If a removed node is a BPA1 or BPA2 guess, remove also
              --      the corresponding pair in \(\mathcal B\) (leaving 
              --      \(\mathcal S\) unchanged).
              --   5. For each pair of matching transitions
              --        \(X \xrightarrow {a_i} \gamma_i\), 
              --        \(Y \xrightarrow {a_i} \delta_i\), 
              --      add the (pruning of) node
              --        \((\gamma_i \alpha, \delta_i \beta)\)
              --      as a child of \((X \alpha, Y \beta)\).
              (Unnormed, Unnormed) -> do
                partialFail2 node track q s
              -- 3. If at least one of \(x \alpha\), \(Y \beta\) is normed, then:
              -- 
              --   1. Update \(\mathcal B\) by removing the pair associated with
              --        \(X\), \(Y\).
              --   2. Update \(\mathcal S\) by adding the pair \((X, Y)\).
              --   3. Recursively execute the partial failure routine on
              --        \((X \alpha, Y \beta)\);
              --      i.e., go back to subcase 1, considering node 
              --        \((X \alpha, Y \beta)\) instead of \((\gamma, \delta)\).
              _ -> do
                modifyBasis (Map.insert (x, y) (Bpa2 (α, β)))
                partialFail node' track q s
          -- In any other cases, recursively execute the partial failure 
          -- routine on \((X \alpha, Y \beta)\); i.e., go back to subcase 1,
          -- considering node 
          --   \((X \alpha, Y \beta)\) instead of \((\gamma, \delta)\).
          _ -> do
            partialFail node' track q s
    
    -- TODO: comment (and rename?)
    partialFail2 node track q s =
      case parent node of
        Nothing -> return False
        Just node'@Node{pair = (x : xs, y : ys)} -> do
          let track' = dropWhile (/= (x, y)) track
          modifyBasis (Map.insert (x, y) (Bpa2 (xs, ys)) . filterBasis track')
          modifyVisitedPairs (filterSet track')
          children <- getChildren node'
          q' <- orderAndPruneNodes xs ys (children Seq.>< filterBranchQueue node q)
          basisUpdate track' s q'
      where
        filterBasis ks = Map.filterWithKey (\k _ -> k `elem` ks || uncurry (==) k)

        filterSet ks = Set.filter (\(x, y) -> any ((x, y) `startsWith`) ks)

        startsWith (x : _, y : _) (k1, k2) = k1 == x && k2 == y
        startsWith _              _        = False

        filterBranchQueue node = Seq.filter (\node' -> parent node' /= parent node)
    
    -- TODO: comment
    addMatchingTransitions bpa1Word node@(Node (x : _, y : _) _) rest = do
      children <- if null bpa1Word
        then getChildren node
        else getChildren node >>= orderAndPruneNodes [] bpa1Word
      modifyBasis (Map.insert (x, y) (Bpa1 bpa1Word))
      orderedChildren <- mapM orderNode children
      basisUpdate ((x, y) : track) s (orderedChildren Seq.>< rest)

    -- TODO: comment
    transitionsMatch :: Grammar -> Variable -> Variable -> Bool
    transitionsMatch (Grammar _ ps) x y =
      Map.keysSet (transitions x ps) == Map.keysSet (transitions y ps)

    -- TODO: comment
    computeβ :: Variable -> Variable -> Bisimulation (Maybe Word)
    computeβ x y = computeβ' [x] [y]
      where 
        computeβ' (x : xs) (y : ys) = do
          nx <- normUsingMap Set.empty [x]
          ny <- normUsingMap Set.empty [y]
          case (nx, ny) of
            (Normed nx, Normed ny) -> do
              ns <- gets normMap
              tsx <- sortedTransitions x
              tsy <- sortedTransitions y
              nextNormReducing tsx tsy ns (nx, ny) (xs, ys) >>= \case
                Just (β, [])    -> return (Just β)
                Just (xs', ys') -> computeβ' xs' ys'
                Nothing         -> return Nothing
            _ -> return Nothing
          where
            -- TODO: comment
            sortedTransitions :: Variable -> Bisimulation [(Label, Word)]
            sortedTransitions x = do
              Grammar _ p <- gets grammar
              return $ List.sortBy (comparing fst) $ Map.assocs $ transitions x p
            -- TODO: comment
            nextNormReducing _ [] _ _ _= return Nothing
            nextNormReducing [] _ _ _ _= return Nothing
            nextNormReducing ((a, α) : tsx) tsy map (nx, ny) (xs, ys) =
              case lookup a tsy of
                Just β -> do
                  nα <- normUsingMap Set.empty α
                  nβ <- normUsingMap Set.empty β
                  case (nα, nβ) of
                    (Normed nα, Normed nβ) ->
                      if reducesNorm nx nα && reducesNorm ny nβ
                        then return (Just (α ++ xs, β ++ ys))
                        else nextNormReducing tsx tsy map (nx, ny) (xs, ys)
                    _-> nextNormReducing tsx tsy map (nx, ny) (xs, ys)
                Nothing -> nextNormReducing tsx tsy map (nx, ny) (xs, ys)

-- TODO: comment
pruneNode :: Node -> Bisimulation Node
pruneNode node@Node{pair = (x, y)} = do
  x' <- pruneWord  x
  y' <- pruneWord  y
  return node{pair = (x', y')}
  where
    pruneWord :: Word -> Bisimulation Word
    pruneWord = flip foldrM [] \x xs' -> do
      normUsingMap Set.empty [x] >>= \case
        Unnormed -> return [x]
        Normed _ -> return (x : xs')

-- TODO: comment
enqueuePrunedNode :: Node 
                  -> BranchQueue 
                  -> (Word, Word) 
                  -> Bisimulation BranchQueue
enqueuePrunedNode parent queue (xs, zs) = do
  node <- pruneNode Node{pair = (xs, zs), parent = Just parent}
  if node `elem` queue
    then return queue
    else return (node Seq.<| queue)

-- TODO: comment
orderNode :: Node -> Bisimulation Node
orderNode node@Node{pair} = do
  pair' <- orderPairByNorm pair
  return node{pair = pair'}

-- TODO: comment (can we avoid defining this function?)
addPairBpa2ToBasis :: Node 
                   -> BranchQueue 
                   -> (Word, Word) 
                   -> [(Variable, Variable)] 
                   -> Set.Set (Variable, Variable) 
                   -> Bisimulation Bool
addPairBpa2ToBasis node@Node{pair = (x : xs, y : ys)} rest bpa2Word track s = do
  children <- getChildren node
  modifyBasis (Map.insert (x, y) (Bpa2 bpa2Word))
  q <- orderAndPruneNodes xs ys (children Seq.>< rest)
  basisUpdate ((x, y) : track) s q

-- TODO: comment
orderPairByNorm :: (Word, Word) -> Bisimulation (Word, Word)
orderPairByNorm = \case
  ([], ys) -> return (ys, [])
  (xs, []) -> return (xs, [])
  (xs@(x : _), ys@(y : _)) -> do
    norm_v1 <- normUsingMap Set.empty [x]
    norm_v2 <- normUsingMap Set.empty [y]
    return $ case (norm_v1,norm_v2) of
      (Unnormed, _) -> (xs, ys)
      (_, Unnormed) -> (ys, xs)
      (Normed n1, Normed n2)
        | n1 > n2   -> (xs, ys)
        | n2 > n1   -> (ys, xs)
        | x <= y    -> (xs, ys)
        | otherwise -> (ys, xs)

-- TODO: comment
getChildren :: Node -> Bisimulation BranchQueue
getChildren  node@Node{pair = (x : _, y : _)} = do
  Grammar _ prods <- gets grammar
  let tsx = transitions x prods
      tsy = transitions y prods
  foldM (enqueuePrunedNode node) Seq.empty $
    map (\l -> (Map.findWithDefault [] l tsx, Map.findWithDefault [] l tsy))
        (List.sort (Map.keys tsx))

-- TODO: comment
orderAndPruneNodes :: Word -> Word -> BranchQueue -> Bisimulation BranchQueue
orderAndPruneNodes xs ys = mapM \node@Node{pair = (xs', ys')} -> do
  pair <- orderPairByNorm (xs' ++ xs, ys' ++ ys)
  pruneNode node{pair}
