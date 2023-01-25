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
  , mkList
  , mkCons
  , mkNil
  , mkSelect
  , mkCollect
  , mkSend
  , mkReceive
  , mkClose
  , mkFork
  , mkError
  , mkUndefined
  ) where
 

import Syntax.Base


mk :: String -> Span -> Variable
mk = flip mkVar

mkWild,mkOr,mkAnd,mkPlus,mkMinus,mkTimes,mkDiv,mkPower,mkNeg :: Span -> Variable
 
mkWild = mk "_"
mkOr = mk "(||)"
mkAnd = mk "(&&)"
mkPlus = mk "(+)"
mkMinus = mk "(-)"
mkTimes = mk "(*)"
mkDiv = mk "(/)"
mkPower = mk "(^)"
mkNeg = mk "negate"

mkList,mkCons,mkNil :: Span -> Variable
mkList = mk "[Int]"
mkCons = mk "(::)"
mkNil  = mk "[]"

mkSelect,mkCollect,mkSend,mkReceive,mkClose,mkFork :: Span -> Variable
mkSelect = mk "select"
mkCollect = mk "collect"
mkSend = mk "send"
mkReceive = mk "receive"
mkClose = mk "close"
mkFork = mk "fork"

mkError, mkUndefined :: Variable
mkError = mk "error" defaultSpan
mkUndefined = mk "undefined" defaultSpan


-- TODO: mk for all builtin functions in Interpreter.Builtin
