module Util.Error
  ( ErrorType(..)
  , showErrors
  , internalError
  ) where

import           Parse.Unparser
import           Syntax.Base
import qualified Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.AST
import qualified Syntax.Type as T
import           Util.Message

import           Data.Either.Extra (fromEither, isLeft)
import qualified Data.Map as Map
import           System.FilePath

-- | Internal errors

internalError :: (Show a, Located a) => String -> a -> b
internalError fun syntax =
  error
    $  show (getSpan syntax)
    ++ ": Internal error at "
    ++ fun
    ++ ": "
    ++ show syntax

-- | Format errors
      
showErrors :: Stylable -> String -> ErrorType -> String
showErrors sty f err =
  let mod = trimModule f (defModule $ getSpan err) in
  let base = replaceBaseName f (fromEither mod) in
  let modEither = if isLeft mod then Left base else Right $ showModuleName (getSpan err) in    
    title err sty (getSpan err) base ++ "\n  " ++ msg err sty modEither
  where
    trimModule f mod
      | null mod                = Left $ takeBaseName f
      | isExtensionOf "fst" mod = Left $ takeBaseName mod
      | otherwise               = Right mod

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
  | MissingModHeader Span FilePath
  -- ParseUtils
  | MultipleFieldDecl Span Span Variable
  | RedundantPMatch Span Variable
  | DuplicateVar Span String Variable Span  -- string is the variable description: type or program
  | DuplicateFieldInDatatype Span Variable Span
  | MissingChoices Span [Variable] Span
  | MultipleDeclarations Span Variable Span
  | MultipleTypeDecl Span Variable Span
  | MultipleFunBindings Span Variable Span
  -- Elab
  | ConflictChoiceCons Span Variable Span
  | DifNumberOfArguments Span Variable 
  | InvalidVariablePatternChan Span Variable
  | TypeVarOutOfScope Span Variable
  | FuctionLacksSignature Span Variable
  | WrongNumberOfArguments Span Variable Int Int T.Type
  -- Duality
--  | DualOfNonRecVar Span  T.Type
  | DualOfNonSession Span T.Type
  -- TypeCheck
  | SignatureLacksBinding Span Variable T.Type
  | MainNotDefined Span Variable
  | UnrestrictedMainFun Span Variable T.Type K.Kind
  | LinearFunctionNotConsumed Span [(Variable, T.Type)]
  -- Kinding
  | TypeVarNotInScope Span Variable -- Duplicated: TypeVarOutOfScope
  | TypeNotContractive Span T.Type Variable
  | CantMatchKinds Span K.Kind K.Kind T.Type
  | ExpectingSession Span T.Type K.Kind
  -- Typing
  | TypeAbsBodyNotValue Span E.Exp E.Exp
  | VarOrConsNotInScope Span Variable
  | LinProgVar Span Variable T.Type K.Kind
  | NonEquivTypes Span T.Type T.Type E.Exp
  | NonEquivEnvsInBranch Span Signatures Signatures E.Exp
  | NonEquivEnvsInUnFun Span Signatures Signatures E.Exp
  | DataConsNotInScope Span Variable
  | WrongNumOfCons Span Variable Int [Variable] E.Exp
  | ExtractError Span String E.Exp T.Type
  | BranchNotInScope Span Variable T.Type
  | UnendedSession Span T.Type K.Kind
  -- Runtime errors
  | ErrorFunction Span String
  | UndefinedFunction Span
  | RuntimeError Span String
  deriving Show

instance Located ErrorType where
  getSpan (FileNotFound _                  ) = defaultSpan
  getSpan (WrongFileExtension _            ) = defaultSpan
  getSpan (LexicalError p _                ) = p
  getSpan (PrematureEndOfFile p            ) = p
  getSpan (ParseError p _                  ) = p
  getSpan (NameModuleMismatch p _ _        ) = p
  getSpan (ImportNotFound p _ _            ) = p
  getSpan (MissingModHeader p _            ) = p
  getSpan (MultipleFieldDecl p _ _         ) = p
  getSpan (RedundantPMatch   p _           ) = p
  getSpan (DuplicateVar p _ _ _            ) = p
  getSpan (DuplicateFieldInDatatype p _ _  ) = p
  getSpan (MissingChoices p _ _            ) = p
  getSpan (MultipleDeclarations p _ _      ) = p
  getSpan (MultipleTypeDecl p _ _          ) = p
  getSpan (MultipleFunBindings p _ _       ) = p
  getSpan (ConflictChoiceCons p _ _        ) = p
  getSpan (DifNumberOfArguments p _        ) = p
  getSpan (InvalidVariablePatternChan p _  ) = p
  getSpan (TypeVarOutOfScope p _           ) = p
  getSpan (FuctionLacksSignature p _       ) = p
  getSpan (WrongNumberOfArguments p _ _ _ _) = p 
  getSpan (DualOfNonSession p _            ) = p
  getSpan (SignatureLacksBinding p _ _     ) = p
  getSpan (MainNotDefined p _              ) = p
  getSpan (UnrestrictedMainFun p _ _ _     ) = p
  getSpan (LinearFunctionNotConsumed p _   ) = p
  getSpan (TypeVarNotInScope p _           ) = p
  getSpan (TypeNotContractive p _ _        ) = p
  getSpan (CantMatchKinds p _ _ _          ) = p
  getSpan (ExpectingSession p _ _          ) = p
  getSpan (TypeAbsBodyNotValue p _ _       ) = p
  getSpan (VarOrConsNotInScope p _         ) = p
  getSpan (LinProgVar p _ _ _              ) = p
  getSpan (NonEquivTypes p _ _ _           ) = p
  getSpan (NonEquivEnvsInBranch p _ _ _    ) = p
  getSpan (NonEquivEnvsInUnFun p _ _ _     ) = p
  getSpan (DataConsNotInScope p _          ) = p
  getSpan (WrongNumOfCons p _ _ _ _        ) = p
  getSpan (ExtractError p _ _ _            ) = p
  getSpan (BranchNotInScope p _ _          ) = p
  getSpan (UnendedSession p _ _              ) = p
  getSpan (ErrorFunction p _               ) = p -- defaultSpan
  getSpan (UndefinedFunction p             ) = p
  getSpan (RuntimeError p _                ) = p

  setSpan _ x = x -- TODO: not used at the moment


instance Message ErrorType where
  title _ sty = msgHeader (red sty "error:") sty
  
  msg (FileNotFound f) sty _ =
    "File " ++ style red sty f ++ " does not exist (No such file or directory)"
  msg (WrongFileExtension f) sty _ = 
   "File has not a valid file extension\n\tExpecting: " ++ red sty (quote $ f -<.> "fst") ++
   "\n\tbut got:   " ++ style red sty f
  msg (LexicalError _ tk) sty _ = "Lexical error on input " ++ red sty tk
  msg (PrematureEndOfFile _) _ _ =  "Parse error: Premature end of file"
  msg (ParseError _ x) sty _ = "Parse error on input: " ++ red sty (quote x)

  msg (ImportNotFound _ m f) sty _ =
    "Could not find module " ++ style red sty (showModuleWithDots m) ++
    "\n  Locations searched:\n\t" ++ style red sty f 
  msg (NameModuleMismatch _ m f) sty _ =
    "File name does not match the module name.\n    Module name: " ++
    style red sty (showModuleWithDots m) ++ "\n    Filename:    " ++ style red sty (f -<.> "fst")
  msg (MissingModHeader _ f) sty _ =
    "File " ++ style red sty (f -<.> "fst") ++ " is missing the module header."
  msg (MultipleFieldDecl sp1 sp2 x) sty _ =
    "Multiple declarations of field " ++ style red sty x ++
    " in a choice type.\n\tDeclared at " ++ show sp1 ++ " and " ++ show sp2
  msg (RedundantPMatch _ x) sty _ =
    "Pattern match is redundant\n\t In a case alternative: " ++  style red sty x
  msg (DuplicateVar p tVar x p') sty _ =
    "Conflicting definitions for the " ++ tVar ++ " variable " ++ style red sty x ++
    "\n\tBound at: " ++ show p' ++ " and " ++ show p
  msg (DuplicateFieldInDatatype p pv p') sty _ =
    "Multiple declarations of " ++ style red sty pv ++ " in a datatype declaration" ++
     "\n\tDeclared at: " ++ show p ++ " and " ++ show p'
  msg (MissingChoices p vs p') sty _ = 
    "Declared " ++ fields ++ (prettyList $ map (style red sty) vs) ++ 
    " at: " ++ show p ++ ", but missing on " ++ show p'
    where fields = if length vs == 1 then "field " else "fields "
          prettyList (x:[])   = x
          prettyList (x:y:[]) = x ++ " and " ++ y
          prettyList (x:xs)   = x ++ ", "    ++ prettyList xs
  msg (MultipleDeclarations p pv p') sty _ =
    "Ambiguous occurrence " ++ style red sty pv ++
    "\n\tDeclared in modules: " ++ showModule (showModuleName p') p' ++
    "\n\t                     " ++ showModule (showModuleName p) p
  msg (MultipleTypeDecl p t p') sty _ =
    "Multiple declarations of type " ++ style red sty t ++
    "\n\t Declared in modules: " ++ showModule (showModuleName p') p' ++
    "\n\t                      " ++ showModule (showModuleName p) p
  msg (MultipleFunBindings sp1 x sp2) sty _ =
    "Multiple bindings for function " ++ style red sty x ++
    "\n\t Declared in modules: " ++ showModule (showModuleName sp2) sp2 ++
    "\n\t                      " ++ showModule (showModuleName sp1) sp1
  msg (ConflictChoiceCons p chan p2) sty _ =
    "Confliting definitions between a choice and a constructor " ++
    style red sty (show chan) ++ 
    "\n  Declared in file/module: " ++ showModule (showModuleName p) p ++
    "\n                           " ++ showModule (showModuleName p2) p2 
  msg (DifNumberOfArguments p fun) sty _ =
    "Equations for " ++ style red sty (show fun) ++
    " have different number of arguments " ++
    "\n  Declared in file/module " ++ showModule (showModuleName p) p ++
    ": " ++ red sty (show fun)
  msg (InvalidVariablePatternChan p v) sty _ = 
    "Cannot mix variables with pattern-matching channel choices." ++
    "\n  Declared in file/module " ++ showModule (showModuleName p) p ++
    ": " ++ red sty (show v)
  msg (TypeVarOutOfScope _ x) sty _ = "Type variable not in scope: " ++ style red sty x
  msg (FuctionLacksSignature _ x) sty _ =
    "The binding for function " ++ style red sty x ++ " lacks an accompanying type signature"
  msg (WrongNumberOfArguments p fun exp got t) sty _ =
    "Wrong number of arguments in function " ++ style red sty (show fun) ++
    "\n  expecting " ++ style red sty (show exp) ++
    ", but got " ++ style red sty (show got) ++
    "\n  Declared in file/module " ++ showModule (showModuleName p) p ++
    ":\n  " ++ red sty (show fun ++ " : " ++ show t)
  msg (DualOfNonSession _ t) sty _ = 
    "Dualof applied to a non session type: " ++ style red sty t
  msg (SignatureLacksBinding _ x t) sty _ = 
    "The type signature for "  ++ style red sty x ++
    " lacks an accompanying binding\n\t Type signature: " ++ style red sty t
  msg (MainNotDefined _ main) sty _ =
    "Main function " ++ style red sty main ++ " is not defined"
  msg (UnrestrictedMainFun _ x t k) sty _ = 
    "The type of " ++ style red sty x ++ " must be non linear\n\t Found type " ++
    style red sty t ++ " of kind " ++ style red sty k
  msg (LinearFunctionNotConsumed _ env) sty _ =
    let c = length env 
        plural = if c > 1 then "s" else ""
        verb = if c > 1 then "were" else "was" in
    "Found " ++ show c ++ " top-level linear function" ++ plural ++ " that " ++ verb ++ " not consumed.\n  They are:" ++
    foldl (\acc (k,v) -> let s = getSpan k in
             acc ++ "\n    " ++ defModule s ++ ":" ++ show s ++ ": " ++
             red sty (show k ++ " : " ++ show v)) "" env    
  -- Validation.Kinding
  msg (TypeVarNotInScope _ a) sty _ = "Type variable not in scope: " ++ style red sty a
  msg (TypeNotContractive _ t a) sty _ =
    "Type " ++ style red sty t ++ " is not contractive on type variable " ++
    style red sty a
  msg (CantMatchKinds _ k k' t) sty _ =
    "Couldn't match expected kind " ++ style red sty k ++ "\n\t with actual kind " ++
    style red sty k' ++ "\n\t for type " ++ style red sty t
  msg (ExpectingSession _ t k) sty _ =
    "Expecting a session type\n\t found type " ++ style red sty t ++ " of kind " ++
    style red sty k
-- Validation.Typing
  msg (TypeAbsBodyNotValue _ e e') sty _ =
    "The body of type abstraction " ++ style red sty e ++ "\n                        namely " ++
    style red sty e' ++ "\n                is not a value"
  msg (VarOrConsNotInScope p pv) sty b =
    let styledVar = style red sty pv in
    let modDesc = if isLeft b then "file" else "module" in
    "Variable or data constructor not in scope: " ++ styledVar ++
    -- "\n  In " ++ modDesc ++ ": " ++ fromEither b ++ -- showModule (showModuleName p) p ++
    "\n  (is " ++ styledVar ++ " a linear variable that has been consumed?)" ++
    "\n  (is " ++ styledVar ++ " defined in a module that you forgot to import?)"
  msg (LinProgVar _ x t k) sty _ =
    "Program variable " ++ style red sty x ++ " is linear at the end of its scope\n\t  variable " ++
    style red sty x ++ " is of type " ++ style red sty t ++ " of kind " ++ style red sty k
  msg (NonEquivTypes _ t u e) sty _ =
    "Couldn't match expected type " ++ style red sty t ++ "\n              with actual type " ++
    style red sty u ++"\n                for expression " ++ style red sty e
  msg (NonEquivEnvsInUnFun _ sigs1 sigs2 e) sty _
    | Map.null diff =
      "Linear variable " ++ style red sty var1 ++ " was consumed in the body of an unrestricted function" ++
      "\n\tvariable " ++ style red sty var1 ++ " is of type " ++ style red sty type1 ++
      "\n\t  and the function is " ++ style red sty e ++
      "\n\t(this risks duplicating or discarding the variable! Consider using a linear function instead.)"
    | otherwise = 
      "Linear variable " ++ style red sty var2 ++ " was created in the body of an unrestricted function" ++
      "\n\tvariable " ++ style red sty var2 ++ " is of type " ++ style red sty type2 ++
      "\n\t  and the function is " ++ style red sty e ++
      "\n\t(this risks duplicating or discarding the variable! Consider using a linear function instead.)"
      where diff = sigs2 Map.\\ sigs1
            var1 = head $ Map.keys $ sigs1 Map.\\ sigs2
            type1 = sigs1 Map.! var1
            var2 = head $ Map.keys diff
            type2 = sigs2 Map.! var2
    -- "Couldn't match the final context against the initial context for an unrestricted function" ++
    -- "\n\t The initial context is " ++ style red sty ts (sigs1 {-Map.\\ sigs2-}) ++
    -- "\n\t   the final context is " ++ style red sty ts (sigs2 {-Map.\\ sigs1-}) ++
    -- "\n\t    and the function is " ++ style red sty ts e ++
    -- "\n\t (unrestricted functions cannot update the context)" ++
    -- "\n\t (if you must update the context, consider using a linear function)"
  msg (NonEquivEnvsInBranch _ sigs1 sigs2 e) sty _ =
    "Couldn't match the final contexts in two distinct branches in a case or conditional expression " ++
    "\n\t       One context is " ++ style red sty sigs1 {-(sigs1 Map.\\ sigs2-} ++
    "\n\t         the other is " ++ style red sty sigs2 {-(sigs2 Map.\\ sigs1-} ++
    "\n\tand the expression is " ++ style red sty e ++
    "\n\t(was a variable consumed in one branch and not in the other?)" ++
    "\n\t(is there a variable with different types in the two contexts?)"
  msg (DataConsNotInScope _ x) sty _ = "Data constructor " ++ style red sty x ++ " not in scope."
  msg (WrongNumOfCons _ x i xs e) sty _ =
    "The constructor " ++ style red sty x ++ " should have " ++ red sty (show i) ++
    " arguments, but has been given " ++ red sty (show $ length xs) ++
    "\n\t In the pattern (" ++ show (startPos $ getSpan x) ++ " - " ++ show (endPos $ getSpan e)  ++ "): " ++
    red sty (show x ++ " " ++ unwords (map show xs) ++ " -> " ++ show e)
  msg (ExtractError _ s e t) sty _ = 
    "Expecting " ++ s ++ " type for expression " ++ style red sty e ++
    "\n                      " ++ replicate (length s) ' ' ++
    "found type " ++ style red sty t
  msg (BranchNotInScope _ x t) sty _ =
    "Choice branch not in scope.\n\t Branch " ++ style red sty x ++
    " is not present in the internal choice type " ++ style red sty t ++
    "\n\t Defined at: " ++ show (getSpan t)
  msg (UnendedSession s t k) sty _ =
    "Session type created with new does not reach an End\n\t" ++
    "In type: " ++ style red sty (show t) ++ "\n\t" ++
    "With kind: " ++ style red sty (show k)
--  Runtime
  msg (ErrorFunction s e) _ _ = -- TODO: This one is from the point of view of the callee not the caller
    e ++ "\n  error, called at module" ++ defModule s ++ ":" ++ show (startPos s)
  msg (UndefinedFunction s) _ _ = 
    "undefined function, called at " ++ defModule s ++ ":" ++ show (startPos s)
  msg (RuntimeError _ e) _ _ = "Exception: " ++ e
