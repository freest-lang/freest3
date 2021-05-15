module Util.Err where


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
  | MultipleTypeDecl Pos ProgVar Pos
  | MultipleFunBindings Pos ProgVar Pos
  -- Elab
  | TypeVarOutOfScope Pos TypeVar
  | FuctionLacksSignature Pos ProgVar
  -- Duality
  | DualOfRecVar Pos TypeVar
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
  | ExpectingArrow Pos E.Exp T.Type
  | ExpectingPair Pos E.Exp T.Type
  | ExpectingPoly Pos E.Exp T.Type
  | ExpectingMessage Pos String E.Exp T.Type -- In/Out (all the others will look like this one)
  | ExpectingChoice Pos String E.Exp T.Type -- Internal/External
  | ExpectingData Pos E.Exp T.Type
  | BranchNotInScope Pos ProgVar T.Type


formatError :: ErrorType -> String -> TypeOpsEnv -> String
formatError (LexicalError p t) f tops =
   formatErrorMessage tops p f [Error "Lexical error on input",
                   Error $ "\ESC[91m" ++ t ++ "\ESC[0m"]
-- Parser.y   
formatError (PrematureEndOfFile p) f tops =
   formatErrorMessage tops p f
     [Error "Parse error:", Error "\ESC[91mPremature end of file\ESC[0m"]
formatError (ParseError p x) f tops =
  formatErrorMessage tops p f
    [Error "Parse error on input", Error $ "\ESC[91m'" ++ x ++ "'\ESC[0m"]
-- Parse.ParseUtils    
formatError (MultipleFieldDecl p pv) f tops =
  formatErrorMessage tops p f
    [ Error "Multiple declarations of field"
    , Error pv
    , Error "\n\t in a choice type"    ]
formatError (RedundantPMatch p pv) f tops =
   formatErrorMessage tops p f
     [ Error "Pattern match is redundant"
     , Error "\n\t In a case alternative:"
     , Error pv
     ]
formatError (DuplicatePVar p pv p') f tops =
   formatErrorMessage tops p f
     [ Error "Conflicting definitions for program variable"
     , Error pv
     , Error "\n\t Bound at:"
     , Error $ show p
     , Error "\n\t          "
     , Error $ show p'
     ]
formatError (DuplicateTVar p tv p') f tops =
  formatErrorMessage tops p f
     [ Error "Conflicting definitions for type variable"
     , Error tv
     , Error "\n\t Bound at: "
     , Error (show p)
     , Error "\n\t           "
     , Error (show p')
     ]
formatError (DuplicateFieldInDatatype p pv) f tops =
  formatErrorMessage tops p f
    [ Error "Multiple declarations of"
    , Error pv
    , Error "\n\t in a datatype declaration"
    ]
formatError (MultipleDatatypeDecl p pv p') f tops =
  formatErrorMessage tops p f
    [ Error "Multiple declarations of"
    , Error pv
    , Error "\n\t Declared at:"
    , Error p
    , Error "\n\t             "
    , Error p'
    ]
formatError (MultipleTypeDecl p pv p') f tops =
  formatErrorMessage tops p f
    [ Error "Multiple declarations of"
    , Error pv
    , Error "\n\t Declared at:"
    , Error p
    , Error "\n\t             "
    , Error p'
    ]
formatError (MultipleFunBindings p pv p') f tops =
  formatErrorMessage tops p f
    [ Error "Multiple bindings for function"
    , Error pv
    , Error "\n\t Declared at:"
    , Error p
    , Error "\n\t             "
    , Error p'
    ]
-- Elaboration.Elaboration
formatError (TypeVarOutOfScope p tv) f tops =
  formatErrorMessage tops p f [Error "Type variable not in scope:", Error tv]
formatError (FuctionLacksSignature p pv) f tops =
  formatErrorMessage tops p f
    [ Error "The binding for function"
    , Error pv
    , Error "lacks an accompanying type signature"
    ]   
-- Elaboration.Duality
formatError (DualOfRecVar p tv) f tops =
  formatErrorMessage tops p f
    [Error "Cannot compute the dual of a non-recursion variable:", Error tv]
formatError (DualOfNonSession p t) f tops =
  formatErrorMessage tops p f
    [Error "Dualof applied to a non session type:", Error t]
-- Validation.TypeChecking
formatError (SignatureLacksBinding p pv t) f tops =
  formatErrorMessage tops p f
    [ Error "The type signature for"
    , Error pv
    , Error "lacks an accompanying binding\n"
    , Error "\t Type signature:"
    , Error $ t
    ]
formatError (MainNotDefined p pv) f tops =
  formatErrorMessage tops p f
    [Error "Function", Error pv, Error "is not defined"]    
formatError (UnrestrictedMainFun p pv t k) f tops =
  formatErrorMessage tops p f
    [ Error "The type of"
    , Error pv
    , Error "must be non linear"     
    , Error "\n\t found type"
    , Error t
    , Error "of kind"
    , Error k
    ]
-- Validation.Kinding
formatError (TypeVarNotInScope p tv) f tops =
  formatErrorMessage tops p f [Error "Type variable not in scope:", Error tv]
formatError (TypeNotContractive p t tv) f tops =
  formatErrorMessage tops p f
    [Error "Type", Error t, Error "is not contractive on type variable", Error tv]
formatError (CantMatchKinds p k k' t) f tops =
  formatErrorMessage tops p f
    [ Error "Couldn't match expected kind"
    , Error k
    , Error "\n\t with actual kind"
    , Error k'
    , Error "\n\t for type"
    , Error t
    ]
formatError (ExpectingSession p t k) f tops =
  formatErrorMessage tops p f
    [ Error "Expecting a session type\n"
    , Error "\t found type"
    , Error t
    , Error "of kind"
    , Error k
    ]

-- Validation.Typing
formatError (TypeAbsBodyNotValue p e e') f tops =
  formatErrorMessage tops p f
    [ Error "The body of type abstraction"
    , Error e, Error "\n\t                       namely"
    , Error e'
    , Error "\n\t               is not a value"]
formatError (VarOrConsNotInScope p pv) f tops =
  formatErrorMessage tops p f
    [ Error "Variable or data constructor not in scope:"
    , Error pv
    , Error "\n\t (is"
    , Error pv
    , Error "a linear variable that has been consumed?)"
    ]
formatError (LinProgVar p pv t k) f tops =
  formatErrorMessage tops p f
    [ Error "Program variable", Error pv
    , Error "is linear at the end of its scope\n\t variable", Error pv
    , Error "is of type", Error t
    , Error "of kind", Error k
    ]
formatError (PartialApplied p e s) f tops =
  formatErrorMessage tops p f
    [ Error "Ooops! You're asking too much. I cannot type a partially applied"
    , Error e, Error "\n\t Consider applying"
    , Error e, Error $ "to an expression denoting a " ++ s ++ "."
    ]
formatError (NonEquivTypes p t u e) f tops =
  formatErrorMessage tops p f
    [ Error "Couldn't match expected type"
    , Error t
    , Error "\n\t             with actual type"
    , Error u
    , Error "\n\t               for expression"
    , Error e
    ]
formatError (NonEquivEnvs p branching vEnv vEnv' e) f tops =
  formatErrorMessage tops p f
    [ Error "I have reached the end of"
    , Error branching
    , Error "expression and found two distinct typing environments."
    , Error "\n\t     The contexts are"
    , Error (vEnv Map.\\ vEnv')
    , Error "\n\t                  and"
    , Error (vEnv' Map.\\ vEnv)
    , Error "\n\tand the expression is"
    , Error e
    , Error "\n\t(was a variable consumed in one branch and not in the other?)"
    , Error
      "\n\t(is there a variable with different types in the two environments?)"
    ]
formatError (NonExhaustiveCase p fm tm) f tops =
  formatErrorMessage tops p f
    [ Error "Wrong number of constructors\n\tThe expression has"
    , Error $ Map.size fm
    , Error "constructor(s)\n\tbut the type has"
    , Error $ Map.size tm
    , Error "constructor(s)\n\tin case "
    , Error $ "\ESC[91m{" ++ showFieldMap fm ++ "}\ESC[0m"
    ]
formatError (DataConsNotInScope p pv) f tops =
  formatErrorMessage tops p f [Error "Data constructor", Error pv, Error "not in scope"]  
formatError (WrongNumOfCons p pv i i' s) f tops =
  formatErrorMessage tops p f
    [ Error "The constructor", Error pv
    , Error "should have", Error i
    , Error "arguments, but has been given", Error i'
    , Error "\n\t In the pattern:"
    , Error s
    ]
-- Validation.Extract -- Join all (except the last)
formatError (ExpectingArrow p e t) f tops =
  formatErrorMessage tops p f
    [ Error "Expecting an arrow type for expression"
    , Error e
    , Error "\n\t                             found type"
    , Error t
    ]
formatError (ExpectingPair p e t) f tops =
  formatErrorMessage tops p f
    [ Error "Expecting a pair type for expression"
    , Error e
    , Error "\n\t found type"
    , Error t
    ]
formatError (ExpectingPoly p e t) f tops =
  formatErrorMessage tops p f
    [ Error "Expecting a polymorphic type for expression"
    , Error e
    , Error "\n\t found type"
    , Error t
    ]
formatError (ExpectingMessage p s e t) f tops =
  formatErrorMessage tops p f
    [ Error $ "Expecting an " ++ s ++ " type for expression"
    , Error e
    , Error "\n\t found type"
    , Error t
    ]

formatError (ExpectingChoice p s e t) f tops =
  formatErrorMessage tops p f
    [ Error $ "Expecting an " ++ s ++ " choice type for expression"
    , Error e
    , Error "\n\t found type"
    , Error t
    ]
formatError (ExpectingData p e t) f tops =
  formatErrorMessage tops p f
    [ Error "Expecting a datatype for expression"
    , Error e
    , Error "\n\t found type"
    , Error t
    ]
formatError (BranchNotInScope p pv t) f tops =
  formatErrorMessage tops p f
   [Error "Branch", Error pv, Error "not present in internal choice type", Error t]
