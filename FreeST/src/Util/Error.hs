module Util.Error
  ( ErrorType(..)
  , formatError
  , internalError
  )
where

import           Syntax.Base
import           Syntax.ProgramVariable
import           Syntax.TypeVariable
import           Syntax.Kind
import           Syntax.Program
import qualified Syntax.Type                   as T
import qualified Syntax.Expression             as E
import           Util.PrettyError
import           Util.ErrorMessage
import qualified Data.Map                      as Map
import           Data.Maybe
import           Parse.Unparser                 ( showFieldMap ) -- temporary


-- Warnings
-- FreeST.hs:82:
   -- styleCyan "warning: " ++ "Couldn't find prelude; proceeding without it" 
-- CmdLine:72:
   -- multiple files provided

-- | Internal errors

internalError :: (Show a, Position a) => String -> a -> b
internalError fun syntax =
  error
    $  show (pos syntax)
    ++ ": Internal error at "
    ++ fun
    ++ ": "
    ++ show syntax

-- | Format errors

formatError :: Maybe String -> TypeOpsEnv -> ErrorType -> String
formatError mFile tops err = format (pos err) (errorMsg err)
 where
  f = fromMaybe "FreeST" mFile
  format p e = formatHeader f p ++ formatBody tops e

-- | Errors

data ErrorType =
  -- CmdLine
    FileNotFound FilePath
  | NoInputFile
  -- Lexer.x
  -- Token, circular import (move to other module)
  | LexicalError Pos String
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
  | WrongNumOfCons Pos ProgVar Int [ProgVar] E.Exp
  | ExtractError Pos String E.Exp T.Type
  | BranchNotInScope Pos ProgVar T.Type
  -- Builtin
  | ErrorFunction String
  deriving Show


instance Position ErrorType where
  pos (FileNotFound _)               = defaultPos
  pos NoInputFile                    = defaultPos
  pos (LexicalError p _            ) = p
  pos (PrematureEndOfFile p        ) = p
  pos (ParseError        p _       ) = p
  pos (MultipleFieldDecl p _       ) = p
  pos (RedundantPMatch   p _       ) = p
  pos (DuplicatePVar p _ _         ) = p
  pos (DuplicateTVar p _ _         ) = p
  pos (DuplicateFieldInDatatype p _) = p
  pos (MultipleDatatypeDecl p _ _  ) = p
  pos (MultipleTypeDecl     p _ _  ) = p
  pos (MultipleFunBindings  p _ _  ) = p
  pos (TypeVarOutOfScope     p _   ) = p
  pos (FuctionLacksSignature p _   ) = p
  pos (DualOfNonRecVar       p _   ) = p
  pos (DualOfNonSession      p _   ) = p
  pos (SignatureLacksBinding p _ _ ) = p
  pos (MainNotDefined p _          ) = p
  pos (UnrestrictedMainFun p _ _ _ ) = p
  pos (TypeVarNotInScope p _       ) = p
  pos (TypeNotContractive p _ _    ) = p
  pos (CantMatchKinds p _ _ _      ) = p
  pos (ExpectingSession    p _ _   ) = p
  pos (TypeAbsBodyNotValue p _ _   ) = p
  pos (VarOrConsNotInScope p _     ) = p
  pos (LinProgVar p _ _ _          ) = p
  pos (PartialApplied p _ _        ) = p
  pos (NonEquivTypes p _ _ _       ) = p
  pos (NonEquivEnvs p _ _ _ _      ) = p
  pos (NonExhaustiveCase p _ _     ) = p
  pos (DataConsNotInScope p _      ) = p
  pos (WrongNumOfCons p _ _ _ _    ) = p
  pos (ExtractError p _ _ _        ) = p
  pos (BranchNotInScope p _ _      ) = p
  pos (ErrorFunction _             ) = defaultPos

errorMsg :: ErrorType -> [ErrorMessage]
-- CmdLine
errorMsg (FileNotFound f) =
  [ Error "File", Error $ '\'' : f ++ "'"
  , Error "does not exist (No such file or directory)"]
errorMsg NoInputFile =
  [ Error "freest: no input files\n\t"
  , Error "Usage: For basic information, try the '--help' option."]
-- Lexer  
errorMsg (LexicalError _ t) =
  [Error "Lexical error on input", Error $ "\ESC[91m" ++ t ++ "\ESC[0m"]
-- Parser.y   
errorMsg (PrematureEndOfFile _) =
  [Error "Parse error:", Error "\ESC[91mPremature end of file\ESC[0m"]
errorMsg (ParseError _ x) =
  [Error "Parse error on input", Error $ "\ESC[91m'" ++ x ++ "'\ESC[0m"]
-- Parse.ParseUtils    
errorMsg (MultipleFieldDecl _ pv) =
  [ Error "Multiple declarations of field", Error pv, Error "\n\t in a choice type"]
errorMsg (RedundantPMatch _ pv) =
  [ Error "Pattern match is redundant", Error "\n\t In a case alternative:", Error pv]
errorMsg (DuplicatePVar p pv p') =
  [ Error "Conflicting definitions for program variable", Error pv
  , Error "\n\t Bound at:", Error $ show p, Error "\n\t          "
  , Error $ show p' ]
errorMsg (DuplicateTVar p tv p') =
  [ Error "Conflicting definitions for type variable", Error tv
  , Error "\n\t Bound at: ", Error (show p)
  , Error "\n\t           ", Error (show p') ]
errorMsg (DuplicateFieldInDatatype _ pv) =
  [ Error "Multiple declarations of", Error pv, Error "\n\t in a datatype declaration"]
errorMsg (MultipleDatatypeDecl p pv p') =
  [ Error "Multiple declarations of", Error pv, Error "\n\t Declared at:"
  , Error p, Error "\n\t             ", Error p']
errorMsg (MultipleTypeDecl p t p') =
  [ Error "Multiple declarations of type", Error t, Error "\n\t Declared at:"
  , Error p, Error "\n\t             ", Error p']
errorMsg (MultipleFunBindings p pv p') =
  [ Error "Multiple bindings for function", Error pv, Error "\n\t Declared at:"
  , Error p, Error "\n\t             ", Error p' ]
-- Elaboration.Elaboration
errorMsg (TypeVarOutOfScope _ tv) = [ Error "Type variable not in scope:", Error tv]
errorMsg (FuctionLacksSignature _ pv) =
  [ Error "The binding for function", Error pv
  , Error "lacks an accompanying type signature"]
-- Elaboration.Duality
errorMsg (DualOfNonRecVar _ t) =
  [Error "Cannot compute the dual of a non-recursion variable:", Error t]
errorMsg (DualOfNonSession _ t) = [Error "Dualof applied to a non session type:", Error t]
-- Validation.TypeChecking
errorMsg (SignatureLacksBinding _ pv t) =
  [ Error "The type signature for", Error pv, Error "lacks an accompanying binding\n"
  , Error "\t Type signature:", Error t ]
errorMsg (MainNotDefined _ pv) = [Error "Main function", Error pv, Error "is not defined"]
errorMsg (UnrestrictedMainFun _ pv t k) =
  [ Error "The type of"    , Error pv, Error "must be non linear"
  , Error "\n\t found type", Error t, Error "of kind", Error k]
-- Validation.Kinding
errorMsg (TypeVarNotInScope _ tv) = [Error "Type variable not in scope:", Error tv]
errorMsg (TypeNotContractive _ t tv) =
  [Error "Type", Error t, Error "is not contractive on type variable", Error tv]
errorMsg (CantMatchKinds _ k k' t) =
  [ Error "Couldn't match expected kind", Error k, Error "\n\t with actual kind", Error k'
  , Error "\n\t for type", Error t ]
errorMsg (ExpectingSession _ t k) =
  [ Error "Expecting a session type\n", Error "\t found type", Error t, Error "of kind", Error k]
-- Validation.Typing
errorMsg (TypeAbsBodyNotValue _ e e') =
  [ Error "The body of type abstraction", Error e
  , Error "\n\t                       namely", Error e'
  , Error "\n\t               is not a value"]
errorMsg (VarOrConsNotInScope _ pv) =
  [ Error "Variable or data constructor not in scope:", Error pv
  , Error "\n\t (is", Error pv, Error "a linear variable that has been consumed?)"]
errorMsg (LinProgVar _ pv t k) =
  [ Error "Program variable", Error pv
  , Error "is linear at the end of its scope\n\t variable"
  , Error pv, Error "is of type", Error t, Error "of kind", Error k]
errorMsg (PartialApplied _ e s) =
  [ Error "Ooops! You're asking too much. I cannot type a partially applied"
  , Error e, Error "\n\t Consider applying", Error e
  ,  Error $ "to an expression denoting a " ++ s ++ "."]
errorMsg (NonEquivTypes _ t u e) =
  [ Error "Couldn't match expected type", Error t
  , Error "\n\t             with actual type", Error u
  , Error "\n\t               for expression", Error e]
errorMsg (NonEquivEnvs _ branching vEnv vEnv' e) =
  [ Error "I have reached the end of", Error branching
  , Error "expression and found two distinct typing environments."
  , Error "\n\t The contexts are", Error (vEnv Map.\\ vEnv')
  , Error "\n\t              and", Error (vEnv' Map.\\ vEnv)
  , Error "\n\t and the expression is", Error e
  , Error "\n\t (are there variables in one environment and not in the other?)"
  , Error "\n\t (is there a variable with different types in the two environments?)"]
errorMsg (NonExhaustiveCase _ fm tm) =
  [ Error "Wrong number of constructors\n\tThe expression has", Error $ Map.size fm
    , Error "constructor(s)\n\tbut the type has", Error $ Map.size tm
    , Error "constructor(s)\n\tin case "
    , Error $ "\ESC[91m{" ++ showFieldMap fm ++ "}\ESC[0m"]
errorMsg (DataConsNotInScope _ pv) =
   [Error "Data constructor", Error pv, Error "not in scope"]
errorMsg (WrongNumOfCons _ pv i pvs e) =
  [ Error "The constructor", Error pv, Error "should have", Error i
  , Error "arguments, but has been given", Error $ length pvs
  , Error "\n\t In the pattern:"
  , Error $ "\ESC[91m" ++ show pv ++ " " ++ unwords (map show pvs)
         ++ " -> " ++ show e ++ "\ESC[0m"]
-- Validation.Extract -- Join all (except the last)
errorMsg (ExtractError _ s e t) =
  [ Error $ "Expecting " ++ s ++ " type for expression", Error e
  , Error $ "\n\t                    " ++ replicate (length s) ' '
  , Error "found type", Error t]
errorMsg (BranchNotInScope _ pv t) =
  [Error "Branch", Error pv, Error "not present in internal choice type", Error t]
-- Builtin
errorMsg (ErrorFunction s) = [Error s]
-- Should I add more info to this error?
-- ex: File: Error s
--       error, called at File:Pos

