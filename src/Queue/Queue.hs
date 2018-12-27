{- |
Module      :  Types
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Queue.Queue
( Queue,
  enqueue,
  --enqueueBatch,
  dequeue,
  front,
  empty,
  isEmpty
) where

import           Syntax.Types
import qualified Data.Set as Set

-- QUEUE

data Queue a = Q ([a], [a])

-- QUEUE OPERATIONS

norm :: ([a],[a]) -> ([a],[a])
norm ([],tr) = (reverse tr, [])
norm (fr,tr) = (fr,tr)

enqueue :: a -> Queue a -> Queue a
enqueue x (Q (fr,tr)) = Q (norm (fr, x:tr))
--
-- enqueueBatch :: Queue -> [(Node, Ancestors)] -> Queue
-- enqueueBatch q xs = foldr enqueue q xs

dequeue :: Queue a -> Queue a
dequeue (Q (x:fr,tr))= Q (norm (fr,tr))
dequeue _ = error "Queue.dequeue: empty queue"

front :: Queue a -> a
front (Q (x:fr, tr)) = x
front _ = error "Queue.front: empty queue"

empty :: Queue a
empty = Q ([],[])

isEmpty :: Queue a -> Bool
isEmpty (Q ([],_)) = True
isEmpty (Q (_,_)) = False
