module Util.Error
  ( ErrorType(..)
  , showErrors
  , internalError
  ) where

import           Parse.Unparser
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.GetTOps
import           Util.Message

import qualified Data.Map as Map
import           System.FilePath

-- | Internal errors

internalError :: (Show a, Spannable a) => String -> a -> b
internalError fun syntax =
  error
    $  show (getSpan syntax)
    ++ ": Internal error at "
    ++ fun
    ++ ": "
    ++ show syntax

-- | Format errors
showErrors :: Stylable -> String -> TypeOpsEnv -> ErrorType -> String
showErrors sty f tops err =
  let base = replaceBaseName f (trimModule f (defModule (getSpan err))) in 
  title err sty (getSpan err) base ++ "\n  " ++ msg err sty tops
  where
    trimModule f mod
      | null mod                = takeBaseName f
      | isExtensionOf "fst" mod = takeBaseName mod
      | otherwise               = mod
      
-- | Errors

data ErrorType =
  -- CmdLine
    FileNotFound FilePath
  | WrongFileExtension FilePath
  -- Lexer.x
  -- Token, circular import (move to other module)
  | LexicalError Span String
  -- Parser.y
  | PrematureEndOfFile Span
  | ParseError Span String -- String should be Token (circular import)
  | NameModuleMismatch Span FilePath FilePath
  | ImportNotFound Span FilePath FilePath
  -- ParseUtils
  | MultipleFieldDecl Span Span Variable
  | RedundantPMatch Span Variable
  | DuplicateVar Span String Variable Span  -- string is the variable description: type or program
  | DuplicateFieldInDatatype Span Variable Span
  | MultipleDeclarations Span Variable Span
  | MultipleTypeDecl Span Variable Span
  | MultipleFunBindings Span Variable Span
  -- Elab
  | TypeVarOutOfScope Span Variable
  | FuctionLacksSignature Span Variable
  -- Duality
--  | DualOfNonRecVar Span  T.Type
  | DualOfNonSession Span T.Type
  -- TypeCheck
  | SignatureLacksBinding Span Variable T.Type
  | MainNotDefined Span Variable
  | UnrestrictedMainFun Span Variable T.Type K.Kind
  -- Kinding
  | TypeVarNotInScope Span Variable -- Duplicated: TypeVarOutOfScope
  | TypeNotContractive Span T.Type Variable
  | CantMatchKinds Span K.Kind K.Kind T.Type
  | ExpectingSession Span T.Type K.Kind
  -- Typing
  | TypeAbsBodyNotValue Span E.Exp E.Exp
  | VarOrConsNotInScope Span Variable
  | LinProgVar Span Variable T.Type K.Kind
  | PartialApplied Span E.Exp String
  | NonEquivTypes Span T.Type T.Type E.Exp
  | NonEquivEnvs Span String VarEnv VarEnv E.Exp
  | DataConsNotInScope Span Variable
  | WrongNumOfCons Span Variable Int [Variable] E.Exp
  | ExtractError Span String E.Exp T.Type
  | BranchNotInScope Span Variable T.Type
  -- Builtin
  | ErrorFunction Span String
  deriving Show

instance Spannable ErrorType where
  getSpan (FileNotFound _)               = defaultSpan
  getSpan (WrongFileExtension _)         = defaultSpan
  getSpan (LexicalError p _              ) = p
  getSpan (PrematureEndOfFile p          ) = p
  getSpan (ParseError        p _         ) = p
  getSpan (NameModuleMismatch p _ _      ) = p
  getSpan (ImportNotFound p _ _          ) = p
  getSpan (MultipleFieldDecl p _ _       ) = p
  getSpan (RedundantPMatch   p _         ) = p
  getSpan (DuplicateVar p _ _ _          ) = p
  getSpan (DuplicateFieldInDatatype p _ _) = p
  getSpan (MultipleDeclarations p _ _  ) = p
  getSpan (MultipleTypeDecl     p _ _  ) = p
  getSpan (MultipleFunBindings  p _ _  ) = p
  getSpan (TypeVarOutOfScope     p _   ) = p
  getSpan (FuctionLacksSignature p _   ) = p
--  getSpan (DualOfNonRecVar       p _   ) = p
  getSpan (DualOfNonSession      p _   ) = p
  getSpan (SignatureLacksBinding p _ _ ) = p
  getSpan (MainNotDefined p _          ) = p
  getSpan (UnrestrictedMainFun p _ _ _ ) = p
  getSpan (TypeVarNotInScope p _       ) = p
  getSpan (TypeNotContractive p _ _    ) = p
  getSpan (CantMatchKinds p _ _ _      ) = p
  getSpan (ExpectingSession    p _ _   ) = p
  getSpan (TypeAbsBodyNotValue p _ _   ) = p
  getSpan (VarOrConsNotInScope p _     ) = p
  getSpan (LinProgVar p _ _ _          ) = p
  getSpan (PartialApplied p _ _        ) = p
  getSpan (NonEquivTypes p _ _ _       ) = p
  getSpan (NonEquivEnvs p _ _ _ _      ) = p
  getSpan (DataConsNotInScope p _      ) = p
  getSpan (WrongNumOfCons p _ _ _ _    ) = p
  getSpan (ExtractError p _ _ _        ) = p
  getSpan (BranchNotInScope p _ _      ) = p
  getSpan (ErrorFunction _ _           ) = defaultSpan


instance Message ErrorType where
  title _  sty = msgHeader (red sty "error:") sty
  
  msg (FileNotFound f) sty ts =
    "File " ++ style red sty ts f ++ " does not exist (No such file or directory)"
  msg (WrongFileExtension f) sty ts = 
   "File has not a valid file extension\n\tExpecting: " ++ red sty (quote $ f -<.> "fst") ++
   "\n\t\tbut got:   " ++ style red sty ts f
  msg (LexicalError _ tk) sty _ = "Lexical error on input " ++ red sty tk
  msg (PrematureEndOfFile _) _ _ =  "Parse error: Premature end of file"
  msg (ParseError _ x) sty _ = "Parse error on input " ++ red sty (quote x)

  msg (ImportNotFound _ m f) sty tops =
    "Could not find module " ++ style red sty tops (showModuleWithDots m) ++
    "\n  Locations searched:\n\t" ++ style red sty tops f 
  msg (NameModuleMismatch _ m f) sty tops =
    "File name does not match the module name.\n    Module name: " ++
    style red sty tops (showModuleWithDots m) ++ "\n    Filename:    " ++ style red sty tops (f -<.> "fst")
  msg (MultipleFieldDecl sp1 sp2 x) sty ts =
    "Multiple declarations of field " ++ style red sty ts x ++
    " in a choice type.\n\tDeclared at " ++ show sp1 ++ " and " ++ show sp2
  msg (RedundantPMatch _ x) sty ts =
    "Pattern match is redundant\n\t In a case alternative: " ++  style red sty ts x
  msg (DuplicateVar p tVar x p') sty ts =
    "Conflicting definitions for the " ++ tVar ++ " variable " ++ style red sty ts x ++
    "\n\tBound at: " ++ show p' ++ " and " ++ show p
  msg (DuplicateFieldInDatatype p pv p') sty ts =
    "Multiple declarations of " ++ style red sty ts pv ++ " in a datatype declaration" ++
     "\n\tDeclared at: " ++ show p ++ " and " ++ show p'
  msg (MultipleDeclarations p pv p') sty ts =
    "Ambiguous occurrence " ++ style red sty ts pv ++
    "\n\tDeclared in modules: " ++ showModule (showModuleName p') p' ++
    "\n\t                     " ++ showModule (showModuleName p) p
  msg (MultipleTypeDecl p t p') sty ts =
    "Multiple declarations of type " ++ style red sty ts t ++
    "\n\t Declared in modules: " ++ showModule (showModuleName p') p' ++
    "\n\t                      " ++ showModule (showModuleName p) p
  msg (MultipleFunBindings sp1 x sp2) sty ts =
    "Multiple bindings for function " ++ style red sty ts x ++
    "\n\t Declared in modules: " ++ showModule (showModuleName sp2) sp2 ++
    "\n\t                      " ++ showModule (showModuleName sp1) sp1
  msg (TypeVarOutOfScope _ x) sty ts = "Type variable not in scope: " ++ style red sty ts x
  msg (FuctionLacksSignature _ x) sty ts =
    "The binding for function " ++ style red sty ts x ++ " lacks an accompanying type signature"
  msg (DualOfNonSession _ t) sty ts = 
    "Dualof applied to a non session type: " ++ style red sty ts t
  msg (SignatureLacksBinding _ x t) sty ts = 
    "The type signature for "  ++ style red sty ts x ++
    " lacks an accompanying binding\n\t Type signature: " ++ style red sty ts t
  msg (MainNotDefined _ main) sty ts =
    "Main function " ++ style red sty ts main ++ " is not defined"
  msg (UnrestrictedMainFun _ x t k) sty ts = 
    "The type of " ++ style red sty ts x ++ " must be non linear\n\t Found type " ++
    style red sty ts t ++ " of kind " ++ style red sty ts k
  -- Validation.Kinding
  msg (TypeVarNotInScope _ a) sty ts = "Type variable not in scope: " ++ style red sty ts a
  msg (TypeNotContractive _ t a) sty ts =
    "Type " ++ style red sty ts t ++ " is not contractive on type variable " ++
    style red sty ts a
  msg (CantMatchKinds _ k k' t) sty ts =
    "Couldn't match expected kind " ++ style red sty ts k ++ "\n\t with actual kind " ++
    style red sty ts k' ++ "\n\t for type " ++ style red sty ts t
  msg (ExpectingSession _ t k) sty ts =
    "Expecting a session type\n\t found type " ++ style red sty ts t ++ " of kind " ++
    style red sty ts k
-- Validation.Typing
  msg (TypeAbsBodyNotValue _ e e') sty ts =
    "The body of type abstraction " ++ style red sty ts e ++ "\n                        namely " ++
    style red sty ts e' ++ "\n                is not a value"
  msg (VarOrConsNotInScope p pv) sty ts =
    let styledVar = style red sty ts pv in
    "Variable or data constructor not in scope: " ++ styledVar ++
    "\n  In module: " ++ showModule (showModuleName p) p ++
    "\n  (is " ++ styledVar ++ " a linear variable that has been consumed?)" ++
    "\n  (is " ++ styledVar ++ " a function defined in other module that is not imported?)"
  msg (LinProgVar _ x t k) sty ts =
    "Program variable " ++ style red sty ts x ++ " is linear at the end of its scope\n\t  variable " ++
    style red sty ts x ++ " is of type " ++ style red sty ts t ++ " of kind " ++ style red sty ts k
  msg (PartialApplied _ e s) sty ts = 
    "Ooops! You're asking too much. I cannot type a partially applied " ++ style red sty ts e ++
    ".\n\t Consider applying " ++ style red sty ts e ++ " to an expression denoting a " ++ s ++ "."
  msg (NonEquivTypes _ t u e) sty ts =
    "Couldn't match expected type " ++ style red sty ts t ++ "\n              with actual type " ++
    style red sty ts u ++"\n                for expression " ++ style red sty ts e
  msg (NonEquivEnvs _ branching vEnv vEnv' e) sty ts =
    "Couldn't match the final context against the initial context for " ++ branching ++
    " expression \n\t The initial context is " ++ style red sty ts (vEnv Map.\\ vEnv') ++
    "\n\t   the final context is " ++ style red sty ts (vEnv' Map.\\ vEnv) ++
    "\n\t  and the expression is " ++ style red sty ts e ++
    "\n\t (was a variable consumed in one branch and not in the other?)" ++
    "\n\t (is there a variable with different types in the two contexts?)"
  msg (DataConsNotInScope _ x) sty ts = "Data constructor " ++ style red sty ts x ++ " not in scope."
  msg (WrongNumOfCons _ x i xs e) sty ts =
    "The constructor " ++ style red sty ts x ++ " should have " ++ red sty (show i) ++
    " arguments, but has been given " ++ red sty (show $ length xs) ++
    "\n\t In the pattern (" ++ show (startPos $ getSpan x) ++ " - " ++ show (endPos $ getSpan e)  ++ "): " ++
    red sty (show x ++ " " ++ unwords (map show xs) ++ " -> " ++ show (getDefault ts e))
  msg (ExtractError _ s e t) sty ts = 
    "Expecting " ++ s ++ " type for expression " ++ style red sty ts e ++
    "\n                      " ++ replicate (length s) ' ' ++
    "found type " ++ style red sty ts t
  msg (BranchNotInScope _ x t) sty ts =
    "Choice branch not in scope.\n\t Branch " ++ style red sty ts x ++
    " is not present in the internal choice type " ++ style red sty ts t ++
    "\n\t Defined at: " ++ show (getSpan t)
--  Builtin
  msg (ErrorFunction s e) _ _ = -- TODO: This one is from the point of view of the callee not the caller
    e ++ "\n  error, called at module" ++ defModule s ++ ":" ++ show (startPos s)
