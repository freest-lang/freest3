module Util.Err
  ( ErrorType(..)
  , formatError) where

-- import Parse.Lexer
import Syntax.Base
import Syntax.ProgramVariable
import Syntax.TypeVariable
import Syntax.Kind
import Syntax.Program
import qualified Syntax.Type as T
import qualified Syntax.Expression as E
import Util.Error
import Util.ErrorMessage
import qualified Data.Map as Map
import Parse.Unparser (showFieldMap) -- temporary
-- Errors

-- Warnings
-- FreeST.hs
-- 72: styleCyan "warning: " ++ "Couldn't find prelude; proceeding without it" 

data ErrorType =
  -- Lexer.x
  -- Token, circular binding (move to other module)
    LexicalError Pos String
  -- Parser.y
  | PrematureEndOfFile Pos
  | ParseError Pos String -- String -> Token
  -- ParseUtils
  | MultipleFieldDecl Pos ProgVar
  | RedundantPMatch Pos ProgVar
  | DuplicatePVar Pos ProgVar Pos -- one pos? (same for the others)
  | DuplicateTVar Pos TypeVar Pos
  | DuplicateFieldInDatatype Pos ProgVar
  | MultipleDatatypeDecl Pos ProgVar Pos
  | MultipleTypeDecl Pos TypeVar Pos
  | MultipleFunBindings Pos ProgVar Pos
  -- Elab
  | TypeVarOutOfScope Pos TypeVar
  | FuctionLacksSignature Pos ProgVar
  -- Duality
  | DualOfNonRecVar Pos  T.Type
  | DualOfNonSession Pos T.Type
  -- TypeCheck
  | SignatureLacksBinding Pos ProgVar T.Type
  | MainNotDefined Pos ProgVar -- Later, a warning
  | UnrestrictedMainFun Pos ProgVar T.Type Kind
  -- Kinding
  | TypeVarNotInScope Pos TypeVar -- Duplicated: TypeVarOutOfScope
  | TypeNotContractive Pos T.Type TypeVar
  | CantMatchKinds Pos Kind Kind T.Type
  | ExpectingSession Pos T.Type Kind
  -- Typing
  | TypeAbsBodyNotValue Pos E.Exp E.Exp
  | VarOrConsNotInScope Pos ProgVar
  | LinProgVar Pos ProgVar T.Type Kind
  | PartialApplied Pos E.Exp String
  | NonEquivTypes Pos T.Type T.Type E.Exp
  | NonEquivEnvs Pos String VarEnv VarEnv E.Exp
  | NonExhaustiveCase Pos E.FieldMap T.TypeMap
  | DataConsNotInScope Pos ProgVar
  | WrongNumOfCons Pos ProgVar Int Int String -- String -> Change Later
  -- Extract (join all of these?)
  | ExtractError Pos String E.Exp T.Type
  -- | ExpectingArrow Pos E.Exp T.Type
  -- | ExpectingPair Pos E.Exp T.Type
  -- | ExpectingPoly Pos E.Exp T.Type
  -- | ExpectingMessage Pos String E.Exp T.Type -- In/Out (all the others will look like this one)
  -- | ExpectingChoice Pos String E.Exp T.Type -- Internal/External
  -- | ExpectingData Pos E.Exp T.Type
  | BranchNotInScope Pos ProgVar T.Type
  deriving Show

formatError :: String -> TypeOpsEnv -> ErrorType -> String
formatError f tops err = uncurry (formatErrorMessage tops f) (errorMsg err)

errorMsg :: ErrorType -> (Pos, [ErrorMessage])
errorMsg (LexicalError p t) =
  (p, [Error "Lexical error on input", Error $ "\ESC[91m" ++ t ++ "\ESC[0m"])
-- Parser.y   
errorMsg (PrematureEndOfFile p) =
  (p, [Error "Parse error:", Error "\ESC[91mPremature end of file\ESC[0m"])
errorMsg (ParseError p x) =
  (p, [Error "Parse error on input", Error $ "\ESC[91m'" ++ x ++ "'\ESC[0m"])
-- Parse.ParseUtils    
errorMsg (MultipleFieldDecl p pv) =
  (p, [ Error "Multiple declarations of field", Error pv,
        Error "\n\t in a choice type"] )
errorMsg (RedundantPMatch p pv) =
  (p, [ Error "Pattern match is redundant"
      , Error "\n\t In a case alternative:", Error pv])
errorMsg (DuplicatePVar p pv p') =
  (p, [ Error "Conflicting definitions for program variable", Error pv
      , Error "\n\t Bound at:", Error $ show p, Error "\n\t          "
      , Error $ show p' ])
errorMsg (DuplicateTVar p tv p') =
  (p, [ Error "Conflicting definitions for type variable", Error tv
      , Error "\n\t Bound at: ", Error (show p)
      , Error "\n\t           ", Error (show p') ])
errorMsg (DuplicateFieldInDatatype p pv) =
  (p, [ Error "Multiple declarations of", Error pv
      , Error "\n\t in a datatype declaration"])
errorMsg (MultipleDatatypeDecl p pv p') =
  (p, [ Error "Multiple declarations of", Error pv, Error "\n\t Declared at:"
      , Error p, Error "\n\t             ", Error p'])
errorMsg (MultipleTypeDecl p t p') =
  (p, [ Error "Multiple declarations of type", Error t, Error "\n\t Declared at:"
      , Error p, Error "\n\t             ", Error p'])
errorMsg (MultipleFunBindings p pv p') =
  (p, [ Error "Multiple bindings for function", Error pv
      , Error "\n\t Declared at:", Error p, Error "\n\t             ", Error p' ])
-- Elaboration.Elaboration
errorMsg (TypeVarOutOfScope p tv) =
  (p, [ Error "Type variable not in scope:", Error tv])
errorMsg (FuctionLacksSignature p pv) =
  (p, [ Error "The binding for function", Error pv
      , Error "lacks an accompanying type signature"])
-- Elaboration.Duality
errorMsg (DualOfNonRecVar p t) =
  (p, [Error "Cannot compute the dual of a non-recursion variable:", Error t])
errorMsg (DualOfNonSession p t) =
  (p, [Error "Dualof applied to a non session type:", Error t])
-- Validation.TypeChecking
errorMsg (SignatureLacksBinding p pv t) =
  (p, [ Error "The type signature for", Error pv, Error "lacks an accompanying binding\n"
      , Error "\t Type signature:", Error $ t ])
errorMsg (MainNotDefined p pv) =
  (p, [ Error "Function", Error pv, Error "is not defined"])
errorMsg (UnrestrictedMainFun p pv t k) =
  (p, [ Error "The type of"    , Error pv, Error "must be non linear",
        Error "\n\t found type", Error t, Error "of kind", Error k])
-- Validation.Kinding
errorMsg (TypeVarNotInScope p tv) =
  (p,  [Error "Type variable not in scope:", Error tv])
errorMsg (TypeNotContractive p t tv) =
  (p, [Error "Type", Error t, Error "is not contractive on type variable", Error tv])
errorMsg (CantMatchKinds p k k' t) =
  (p, [ Error "Couldn't match expected kind", Error k
      , Error "\n\t with actual kind", Error k'
      , Error "\n\t for type", Error t ])
errorMsg (ExpectingSession p t k) =
  (p, [ Error "Expecting a session type\n", Error "\t found type", Error t
      , Error "of kind", Error k])
-- Validation.Typing
errorMsg (TypeAbsBodyNotValue p e e') =
  (p, [ Error "The body of type abstraction", Error e
      , Error "\n\t                       namely", Error e'
      , Error "\n\t               is not a value"])
errorMsg (VarOrConsNotInScope p pv) =
  (p, [ Error "Variable or data constructor not in scope:", Error pv
      , Error "\n\t (is", Error pv, Error "a linear variable that has been consumed?)"])
errorMsg (LinProgVar p pv t k) =
  (p, [ Error "Program variable", Error pv,
        Error "is linear at the end of its scope\n\t variable"
      , Error pv, Error "is of type", Error t, Error "of kind", Error k])
errorMsg (PartialApplied p e s) =
  (p, [ Error "Ooops! You're asking too much. I cannot type a partially applied"
      , Error e, Error "\n\t Consider applying", Error e
      ,  Error $ "to an expression denoting a " ++ s ++ "."])
errorMsg (NonEquivTypes p t u e) =
  (p, [ Error "Couldn't match expected type", Error t
      , Error "\n\t             with actual type", Error u
      , Error "\n\t               for expression", Error e])
errorMsg (NonEquivEnvs p branching vEnv vEnv' e) =
  (p, [ Error "I have reached the end of", Error branching
      , Error "expression and found two distinct typing environments."
      , Error "\n\t     The contexts are", Error (vEnv Map.\\ vEnv')
      , Error "\n\t                  and", Error (vEnv' Map.\\ vEnv)
      , Error "\n\tand the expression is", Error e
      , Error "\n\t(was a variable consumed in one branch and not in the other?)"    
      , Error "\n\t(is there a variable with different types in the two environments?)"])
errorMsg (NonExhaustiveCase p fm tm) =
  (p, [ Error "Wrong number of constructors\n\tThe expression has", Error $ Map.size fm
    , Error "constructor(s)\n\tbut the type has", Error $ Map.size tm
    , Error "constructor(s)\n\tin case "
    , Error $ "\ESC[91m{" ++ showFieldMap fm ++ "}\ESC[0m"])
errorMsg (DataConsNotInScope p pv) =
  (p,  [Error "Data constructor", Error pv, Error "not in scope"])
errorMsg (WrongNumOfCons p pv i i' s) =
  (p, [ Error "The constructor", Error pv, Error "should have", Error i
      , Error "arguments, but has been given", Error i'
      , Error "\n\t In the pattern:", Error s] )
-- Validation.Extract -- Join all (except the last)
errorMsg (ExtractError p s e t) =
  (p, [ Error $ "Expecting " ++ s ++ " type for expression", Error e
      , Error $ "\n\t                    " ++ replicate (length s) ' '
      , Error "found type", Error t])
errorMsg (BranchNotInScope p pv t) =
  (p, [Error "Branch", Error pv, Error "not present in internal choice type", Error t])





-- errorMsg (ExpectingArrow p e t) =
--   (p, 
--     [ Error "Expecting an arrow type for expression"
--     , Error e
--     , Error "\n\t                             found type"
--     , Error t
--     ]
-- errorMsg (ExpectingPair p e t) =
--   (p, 
--     [ Error "Expecting a pair type for expression"
--     , Error e
--     , Error "\n\t found type"
--     , Error t
--     ]
-- errorMsg (ExpectingPoly p e t) =
--   (p, 
--     [ Error "Expecting a polymorphic type for expression"
--     , Error e
--     , Error "\n\t found type"
--     , Error t
--     ]
-- errorMsg (ExpectingMessage p s e t) =
--   (p, 
--     [ Error $ "Expecting an " ++ s ++ " type for expression"
--     , Error e
--     , Error "\n\t found type"
--     , Error t
--     ]

-- errorMsg (ExpectingChoice p s e t) =
--   (p, 
--     [ Error $ "Expecting an " ++ s ++ " choice type for expression"
--     , Error e
--     , Error "\n\t found type"
--     , Error t
--     ]
-- errorMsg (ExpectingData p e t) =
--   (p, 
--     [ Error "Expecting a datatype for expression"
--     , Error e
--     , Error "\n\t found type"
--     , Error t
--     ]
  
