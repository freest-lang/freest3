module Syntax.MkName
  ( mkWild
  , mkOr
  , mkAnd
  , mkPlus
  , mkMinus
  , mkTimes
  , mkDiv
  , mkPower
  , mkNeg
  , mkDollar
  , mkPlusPlus
  , mkPipeGT
  , mkSemi
  , mkTrue
  , mkFalse
  , mkList
  , mkCons
  , mkNil
  , mkTupleLabels
  , mkNew 
  , mkSelect
  , mkCollect
  , mkSend
  , mkReceive
  , mkClose
  , mkFork
  , mkError
  , mkUndefined
  , mkMain 
  ) where
 
import Syntax.Base

mk :: String -> Span -> Variable
mk = flip mkVar

mkWild, mkOr, mkAnd, mkPlus, mkMinus, mkTimes, mkDiv, mkPower, mkNeg, mkDollar, mkPlusPlus, mkPipeGT, mkSemi :: Span -> Variable
 
mkWild = mk "_"
mkOr = mk "(||)"
mkAnd = mk "(&&)"
mkPlus = mk "(+)"
mkMinus = mk "(-)"
mkTimes = mk "(*)"
mkDiv = mk "(/)"
mkPower = mk "(^)"
mkNeg = mk "negate"
mkDollar = mk "($)"
mkPlusPlus = mk "(++)"
mkPipeGT = mk "(|>)"
mkSemi = mk "(;)"

mkTrue, mkFalse :: Span -> Variable 
mkTrue  = mk "True"
mkFalse = mk "False"

mkList, mkCons, mkNil :: Span -> Variable
mkList = mk "[Int]"
mkCons = mk "(::)"
mkNil  = mk "[]"

mkTupleLabels :: [Span -> Variable]
mkTupleLabels = map (mk . show) [0..]

mkNew, mkSelect, mkCollect, mkSend, mkReceive, mkClose, mkFork :: Span -> Variable
mkNew = mk "new"
mkSelect = mk "select"
mkCollect = mk "#collect"
mkSend = mk "send"
mkReceive = mk "receive"
mkClose = mk "close"
mkFork = mk "fork"

mkError, mkUndefined :: Variable
mkError = mk "error" defaultSpan
mkUndefined = mk "undefined" defaultSpan

mkMain :: Variable 
mkMain = mk "main" defaultSpan

-- TODO: mk for all builtin functions in Interpreter.Builtin
