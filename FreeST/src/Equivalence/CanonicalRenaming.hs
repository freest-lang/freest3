


module Equivalence.CanonicalRenaming
  ( canonicallyRename
  )
where

import qualified Data.Set    as Set

import qualified Syntax.Base as B
import qualified Syntax.Type as T


-- TODO: Labelled, dualof, CForall


-- in type t replace b by c. return [c/b]t
varRename :: T.Type -> B.Variable -> B.Variable -> T.Type
varRename (T.Var s a) b c = T.Var s $ if a == b then c else a
varRename (T.Forall s (B.Bind sb a k t)) b c
  | a == b = T.Forall s $ B.Bind sb a k t
  | a /= c = T.Forall s $ B.Bind sb a k $ varRename t b c
  | a == c = T.Forall s $ B.Bind sb a' k $ varRename (varRename t a a') b c
    where a' = head $ filter (`Set.notMember` set) freshVars
          set = Set.union (freeVars t) (Set.singleton c)
varRename (T.Rec s (B.Bind sb a k t)) b c
  | a == b = T.Rec s $ B.Bind sb a k t
  | a /= c = T.Rec s $ B.Bind sb a k $ varRename t b c
  | a == c = T.Rec s $ B.Bind sb a' k $ varRename (varRename t a a') b c
    where a' = head $ filter (`Set.notMember` set) freshVars
          set = Set.union (freeVars t) (Set.singleton c)
varRename (T.Arrow st su t u) b c = T.Arrow st su (varRename t b c) (varRename u b c)
varRename (T.Semi s t u) b c = T.Semi s (varRename t b c) (varRename u b c)
varRename (T.Message s p t) b c = T.Message s p (varRename t b c)
varRename t _ _ = t



freeVars :: T.Type -> Set.Set B.Variable
freeVars (T.Forall _ (B.Bind _ v k t)) = Set.delete v (freeVars t)
freeVars (T.Rec _ (B.Bind _ v k t)) = Set.delete v (freeVars t)
freeVars (T.Arrow _ _ t u) = Set.union (freeVars t) (freeVars u)
freeVars (T.Semi _ t u) = Set.union (freeVars t) (freeVars u)
freeVars (T.Message _ _ t) = freeVars t
freeVars (T.Var s v) = Set.singleton v
freeVars _ = Set.empty



-- ["#1", "#2", ...]
freshVars :: [B.Variable]
freshVars = map (\i -> B.mkVar B.defaultSpan $ "#" ++ show i) [1..]

first :: T.Type -> B.Variable
first t = head $ filter (`Set.notMember` freeVars t) freshVars



-- Ready for testing
canonicallyRename :: T.Type -> T.Type
-- TODO: change type to non-binding canonical forall
canonicallyRename (T.Forall s (B.Bind sb v k t)) = T.Forall s (B.Bind sb cv k ct)
  where
    cv = first t
    ct = varRename t v cv

canonicallyRename (T.Arrow s m t u) = T.Arrow s m (canonicallyRename t) (canonicallyRename u)
canonicallyRename (T.Semi s t u) = T.Semi s (canonicallyRename t) (canonicallyRename u)
canonicallyRename (T.Message s p t) = T.Message s p (canonicallyRename t)
canonicallyRename (T.Rec s (B.Bind sb v k t)) = T.Rec s (B.Bind sb v k (canonicallyRename t))
-- canonicallyRename (Labelled s st tm)
canonicallyRename t = t

