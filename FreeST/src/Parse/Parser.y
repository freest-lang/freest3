{
{-# LANGUAGE TupleSections, NamedFieldPuns #-}
module Parse.Parser
where

import           Parse.Lexer
import           Parse.ParseUtils
import           Syntax.Base
import           Syntax.Expression as E
import qualified Syntax.Kind as K
import           Syntax.Program
import qualified Syntax.Type as T
import           Util.Error
import           Util.FreestState

import           Control.Monad.State
import           Data.Either
import           Data.Functor
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Prelude hiding (span)
import           System.Directory
import           System.FilePath
import Debug.Trace
}

%partial modname Module
%name terms TopLevel
%name expr Exp
%name types Type
%name kinds Kind
%tokentype { Token }
%error { parseError }
%monad { FreestStateT } { (>>=) } { return }

%token
  nl       {TokenNL _}
  where    {TokenWhere _}
  module   {TokenModule _}  
  import   {TokenImport _}  
  Int      {TokenIntT _}
  Char     {TokenCharT _}
  Bool     {TokenBoolT _}
  String   {TokenStringT _}
  '()'     {TokenUnit _}
  '->'     {TokenUnArrow _}
  '-o'     {TokenLinArrow _}
  lambda   {TokenLambda _}
  Lambda   {TokenUpperLambda _}
  Skip     {TokenSkip _}
  '('      {TokenLParen _}
  ')'      {TokenRParen _}
  ','      {TokenComma _}
  '['      {TokenLBracket _}
  ']'      {TokenRBracket _}
  ':'      {TokenColon _}
  ';'      {TokenSemi _}
  '!'      {TokenMOut _}
  '?'      {TokenMIn _}
  '{'      {TokenLBrace _}
  '}'      {TokenRBrace _}
  '=>'     {TokenFArrow _}
  '&&'     {TokenConjunction _}
  '||'     {TokenDisjunction _}
  '/'      {TokenDiv _}
  '&'      {TokenAmpersand _}
  '+'      {TokenPlus _}
  '-'      {TokenMinus _}
  '*'      {TokenTimes _}
  '^'      {TokenRaise _}
  '_'      {TokenWild _}
  '$'      {TokenDollar _}
  '.'      {TokenDot _}
  CMP       {TokenCmp _ _}
  UPPER_ID {TokenUpperId _ _}
  LOWER_ID {TokenLowerId _ _}
  rec      {TokenRec _}
  SU       {TokenSU _}
  SL       {TokenSL _}
  TU       {TokenTU _}
  TL       {TokenTL _}
  MU       {TokenMU _}
  ML       {TokenML _}
  INT      {TokenInt _ _ }
  BOOL     {TokenBool _ _}
  CHAR     {TokenChar _ _}
  STR      {TokenString _ _}
  let      {TokenLet _}
  in       {TokenIn _}
  '='      {TokenEq _}
  data     {TokenData _}
  type     {TokenType _}
  '|'      {TokenPipe _}
  if       {TokenIf _}
  then     {TokenThen _}
  else     {TokenElse _}
  new      {TokenNew _}
  select   {TokenSelect _}
  match    {TokenMatch _}
  with     {TokenWith _}
  case     {TokenCase _}
  of       {TokenOf _}
  forall   {TokenForall _}
  dualof   {TokenDualof _}

-- %nonassoc LOWER_ID UPPER_ID
-- %nonassoc '(' '['
-- %nonassoc '()'
%right in else match case
%nonassoc new
%left '||'       -- disjunction
%left '&&'       -- conjunction
%nonassoc CMP    -- comparison (relational and equality)
%left '+' '-'    -- aditive
%left '*' '/'    -- multiplicative
%right '^'        -- power
%left NEG not    -- unary
%right '.'       -- ∀ a:k . T and μ a:k . T
%right '=>' '->' '-o' ARROW -- λλ a:k => e,  x:T -> e, λ x:T -o e, T -> T and T -o T
%right ';'       -- T;T and e;e
%right MSG       -- !T and ?T
%right dualof
%nonassoc ProgVarWildTBind
%right '$'       -- function call
%left '&'        -- function call

%%
--------------
-- TopLevel --
--------------
  
Module :: { () }
  : module QualifiedUpperId where {% setModuleName $ Just $2 }
  |                               {}

TopLevel :: {}
  : module QualifiedUpperId where NL Import {}
  | Import                                  {}

Import :: { () }
  : import QualifiedUpperId NL Import        {% addImport $2 }
  | Prog                                     {}


QualifiedUpperId :: { FilePath }
  : UPPER_ID '.' QualifiedUpperId  { getText $1 ++ "/" ++ $3}
  | UPPER_ID                       { getText $1 }


-------------
-- Program --
-------------

Prog :: { () }
  : Decl         {}
  | Decl NL Prog {}

NL :: { () }
  : nl NL {}
  | nl    {}

Decl :: { () }
  -- Function signature
  : ProgVar ':' Type {% checkDupProgVarDecl $1 >> addToVEnv $1 $3 }
  -- Function declaration
  | ProgVar ProgVarWildSeq '=' Exp {% checkDupFunDecl $1 >> addToPEnv $1 $2 $4 }
  -- Type abbreviation
  | type KindedTVar TypeDecl {% checkDupTypeDecl (fst $2) >> uncurry addToTEnv $2 $3 }
  -- Datatype declaration
  | data KindedTVar '=' DataCons {% do
      let a = fst $2
      checkDupTypeDecl a
      let bs = typeListToType a $4 :: [(Variable, T.Type)]
      mapM_ (\(c, t) -> addToVEnv c t) bs
      uncurry addToTEnv $2 (T.Almanac (span a) T.Variant (Map.fromList bs))
    }

TypeDecl :: { T.Type }
  : '=' Type { $2 }
  | KindBind TypeDecl { let (a,k) = $1 in T.Forall (span a) (Bind (span k) a k $2) }

DataCons :: { [(Variable, [T.Type])] }
  : DataCon              {% checkDupCons $1 [] >> return [$1] }
  | DataCon '|' DataCons {% checkDupCons $1 $3 >> return ($1 : $3) }
  
DataCon :: { (Variable, [T.Type]) }
  : Constructor TypeSeq { ($1, $2) }

----------------
-- EXPRESSION --
----------------

Exp :: { E.Exp }
  : let ProgVarWild '=' Exp in Exp {% mkSpanPosPos (startPos $ span $1) (endPos $ span $6) >>=
                                       \s -> pure $ E.UnLet s $2 $4 $6 }
  | Exp ';' Exp                    {% mkSpanPosPos (startPos $ span $1) (endPos $ span $3) >>=
                                       \s -> mkSpanPosPos (startPos $ span $1) (endPos $ span $1) >>=
                                       \s' -> pure $ E.UnLet s (mkVar s' "_") $1 $3 }
  | let '(' ProgVarWild ',' ProgVarWild ')' '=' Exp in Exp
                                   {% mkSpanPosPos (startPos $ span $1) (endPos $ span $10) >>=
                                       \s -> pure $ E.BinLet s $3 $5 $8 $10 }
  | if Exp then Exp else Exp       {% mkSpanPosPos (startPos $ span $1) (endPos $ span $6) >>=
                                       \s -> pure $ E.Cond s $2 $4 $6 }
  | new Type                       {% mkSpanPosPos (startPos $ span $1) (endPos $ span $2) >>=
                                       \s -> pure $ E.New s $2 (T.Dualof (negSpan s) $2) }
  | match Exp with '{' MatchMap '}' {% let s' = span $2 in mkSpanPosPos (startPos $ span $1) (endPos $ span $6) >>=
                                       \s -> pure $ E.Case s (E.App s' (E.Var s' (mkVar s' "collect")) $2) $5 }
  | case Exp of '{' CaseMap '}'    {% mkSpanPosPos (startPos $ span $1) (endPos $ span $6) >>=
                                       \s -> pure $ E.Case s $2 $5 }
  | Exp '$' Exp                    {% mkSpanPosPos (startPos $ span $1) (endPos $ span $3) >>=
                                       \s -> pure $ E.App s $1 $3 }
  | Exp '&' Exp                    {% mkSpanPosPos (startPos $ span $1) (endPos $ span $3) >>=
                                       \s -> pure $  E.App s $3 $1 }
  | Exp '||' Exp                   {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s "(||)") $3 }
  | Exp '&&' Exp                   {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s "(&&)") $3 }
  | Exp CMP Exp                    {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s (getText $2)) $3 }
  | Exp '+' Exp                    {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s "(+)") $3 }
  | Exp '-' Exp                    {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s "(-)") $3 }
  | Exp '*' Exp                    {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s "(*)") $3 }
  | Exp '/' Exp                    {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s "(/)") $3 }
  | Exp '^' Exp                    {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ binOp $1 (mkVar s "(^)") $3 }
  | '-' App %prec NEG              {% mkSpanPos (startPos $ span $1) >>= \s -> pure $ unOp (mkVar s "negate") $2 s }
  | '(' Op Exp ')'                 {% mkSpanPosPos (startPos $ span $1) (startPos $ span $4) >>= pure . unOp $2 $3 } -- left section
  | '(' Exp Op ')'                 {% mkSpanPosPos (startPos $ span $1) (startPos $ span $4) >>= pure . unOp $3 $2 } -- right section
  | '(' Exp '-' ')'                {% mkSpanPos (startPos $ span $2) >>= \s -> pure $ unOp (mkVar s "(-)") $2 s } -- right section (-)
  | App                            { $1 }

App :: { E.Exp }
  : App Primary                    {% mkSpanPosPos (startPos $ span $1) (startPos $ span $2) >>= \s -> return $ E.App s $1 $2 }
  | select Constructor             {% mkSpanPosPos (startPos $ span $1) (endPos $ span $2) >>=
                                       \s -> mkSpanPos (startPos $ span $2) >>=
                                       \s1 -> pure $ E.App s (E.Var s (mkVar s1 "select")) (E.Var s1 $2)
                                   }
  | TApp ']'                       { $1 }
  | Primary                        { $1 }
   
Primary :: { E.Exp }
  : INT                            {% let (TokenInt p x) = $1 in flip E.Int x `fmap` mkSpanPos (startPos p) }
  | BOOL                           {% let (TokenBool p x) = $1 in flip E.Bool x `fmap`  mkSpanPos (startPos p) }
  | CHAR                           {% let (TokenChar p x) = $1 in flip E.Char x `fmap` mkSpanPos (startPos p) }
  | STR                            {% let (TokenString p x) = $1 in flip String x `fmap` mkSpanPos (startPos p) }
  | '()'                           {% E.Unit `fmap` mkSpanFromSpan (span $1) }
  | ArbitraryProgVar               {% flip E.Var $1 `fmap` mkSpanFromSpan (span $1) }
  | lambda ProgVarWildTBind Abs
      {% let (m,e) = $3 in mkSpanPosPos (startPos $ span $1) (endPos $ span e) >>=
         \s -> pure $ E.Abs s m (Bind s (fst $2) (snd $2) e) }
  | Lambda KindBind TAbs
      {% let (a,k) = $2 in mkSpanPosPos (startPos $ span $1) (endPos $ span $3) >>=
         \s -> pure $ E.TypeAbs s (Bind s a k $3) }
  | '(' Exp ',' Tuple ')'          {% mkSpanPosPos (startPos $ span $1) (startPos $ span $5) >>= \s -> pure $ E.Pair s $2 $4 }
  | '(' Exp ')'                    { $2 }


Abs :: { (Multiplicity, E.Exp) }
  : Arrow Exp { ($1, $2) }
  | ProgVarWildTBind Abs
      {% let (v, t) = $1 in
         let (m, e) = $2 in
         mkSpanPosPos (startPos $ span v) (endPos $ span e) >>=
          \s -> pure (m, E.Abs s m (Bind s v t e))
      }

TAbs :: { E.Exp }
  : '=>' Exp { $2 }
  | KindBind TAbs
      {% let (a,k) = $1 in mkSpanPosPos (pos a) (pos $2) >>=
         \s -> mkSpanPosPos (pos k) (pos $2) >>=
         \s' -> pure $ E.TypeAbs s (Bind s' a k $2)
      }

TApp :: { E.Exp }
  : App '[' Type     {% mkSpanPosPos (pos $1) (pos $3) >>= \s -> pure $ E.TypeApp s $1 $3 }
  | TApp ',' Type    {% mkSpanPosPos (pos $1) (pos $3) >>= \s -> pure $ E.TypeApp s $1 $3 }

Tuple :: { E.Exp }
  : Exp           { $1 }
  | Exp ',' Tuple {% mkSpanPosPos (pos $1) (pos $3) >>= \s -> pure $ E.Pair s $1 $3 }

MatchMap :: { FieldMap }
  : Match              { uncurry Map.singleton $1 }
  | Match ',' MatchMap {% checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }

Match :: { (Variable, ([Variable], E.Exp)) }
  : ArbitraryProgVar ProgVarWild '->' Exp { ($1, ([$2], $4)) }

CaseMap :: { FieldMap }
  : Case             { uncurry Map.singleton $1 }
  | Case ',' CaseMap {% checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }

Case :: { (Variable, ([Variable], E.Exp)) }
  : Constructor ProgVarWildSeq '->' Exp { ($1, ($2, $4)) }

Op :: { Variable }
   : '||'  {% flip mkVar "(||)" `fmap` mkSpanPos (startPos $ span $1) }
   | '&&'  {% flip mkVar "(&&)" `fmap` mkSpanPos (startPos $ span $1)  }
   | CMP   {% flip mkVar (getText $1) `fmap` mkSpanPos (startPos $ span $1) }
   | '+'   {% flip mkVar "(+)" `fmap` mkSpanPos (startPos $ span $1)  }
   | '*'   {% flip mkVar "(*)" `fmap` mkSpanPos (startPos $ span $1)  }
   | '/'   {% flip mkVar "(/)"  `fmap` mkSpanPos (startPos $ span $1) }
   | '^'   {% flip mkVar "(^)" `fmap` mkSpanPos (startPos $ span $1)  }


----------
-- TYPE --
----------

Type :: { T.Type }
  -- Functional types
  : Int                           {% T.Int `fmap` mkSpanPos (startPos $ span $1) }
  | Char                          {% T.Char `fmap` mkSpanPos (startPos $ span $1) }
  | Bool                          {% T.Bool `fmap` mkSpanPos (startPos $ span $1) }
  | String                        {% T.String `fmap` mkSpanPos (startPos $ span $1) }
  | '()'                          {% T.Unit `fmap` mkSpanPos (startPos $ span $1) }
  | Type Arrow Type %prec ARROW   {% mkSpanPosPos (startPos $ span $1) (endPos $ span $3) >>=
                                   \s -> pure $ T.Arrow s $2 $1 $3 }
  | '(' Type ',' TupleType ')'    {% mkSpanPosPos (startPos $ span $1) (startPos $ span $5) >>=
                                   \s -> pure $ T.Pair s $2 $4 }
  -- Session types
  | Skip                          {% mkSpanPos (startPos $ span $1) >>= pure . T.Skip }
  | Type ';' Type                 {% mkSpanPosPos (startPos $ span $1) (startPos $ span $3) >>=
                                   \s -> pure $ T.Semi s $1 $3 }
  | Polarity Type %prec MSG       {% mkSpanPosPos (startPos $ fst $1) (endPos $ span $2) >>=
                                   \s -> pure $ T.Message s (snd $1) $2 }
  | ChoiceView '{' FieldList '}'  {% mkSpanPosPos (startPos $ fst $1) (endPos $ span $4) >>=
                                   \s -> pure $ T.Almanac s (T.Choice (snd $1)) $3 }
  -- Polymorphism and recursion
  | rec KindBind '.' Type
      {% mkSpanPosPos (startPos $ span $1) (startPos $ span $4) >>= \s -> let (a,k) = $2 in
       pure $ T.Rec s (Bind (span a) a k $4) }
  | forall KindBind Forall        {% let (a,k) = $2 in mkSpanPosPos (startPos $ span $1) (startPos $ span $3) >>= \s ->
                                     pure $ T.Forall s (Bind (span a) a k $3) }
  | TypeVar                       {% mkSpanPos (startPos $ span $1) >>= \s -> pure $ T.Var s $1 }
  -- Type operators
  | dualof Type                   {% flip T.Dualof $2 `fmap` mkSpanPosPos (startPos $ span $1) (startPos $ span $2) }
  | TypeName                      {% flip T.Var $1 `fmap` mkSpanPos (startPos $ span $1) }   -- TODO: remove this one lex
  | '(' Type ')'                  { $2 }

Forall :: { T.Type }
  : '.' Type { $2 }
  | KindBind Forall
      { let (a,k) = $1 in T.Forall (span a) (Bind (span k) a k $2) }

TupleType :: { T.Type }
  : Type               { $1 }
  | Type ',' TupleType { T.Pair (span $1) $1 $3 }

Arrow :: { Multiplicity }
  : '->' { Un  }
  | '-o' { Lin }

Polarity :: { (Span, T.Polarity) }
  : '!' { (span $1, T.Out) }
  | '?' { (span $1, T.In) }

ChoiceView :: { (Span, T.View) }
  : '+' { (span $1, T.Internal) }
  | '&' { (span $1, T.External) }

FieldList :: { T.TypeMap }
  : Field               { uncurry Map.singleton $1 }
  | Field ',' FieldList {% checkDupField (fst $1) $3 >>
                           return (uncurry Map.insert $1 $3) }
                           
Field :: { (Variable, T.Type) }
  : ArbitraryProgVar ':' Type { ($1, $3) }

-- TYPE SEQUENCE

TypeSeq :: { [T.Type] }
  :              { [] }
  | Type TypeSeq { $1 : $2 }

----------
-- KIND --
----------

Kind :: { K.Kind }
  : SU {% K.su `fmap` mkSpanPos (startPos $ span $1) }
  | SL {% K.sl `fmap` mkSpanPos (startPos $ span $1) }
  | TU {% K.tu `fmap` mkSpanPos (startPos $ span $1) }
  | TL {% K.tl `fmap` mkSpanPos (startPos $ span $1) }
  | MU {% K.mu `fmap` mkSpanPos (startPos $ span $1) }
  | ML {% K.ml `fmap` mkSpanPos (startPos $ span $1) }

-- PROGRAM VARIABLE

ArbitraryProgVar :: { Variable }
 : ProgVar     { $1 }
 | Constructor { $1 }

ProgVar :: { Variable }
  : LOWER_ID {% flip mkVar (getText $1) `fmap` mkSpanPos (startPos $ span $1) }

Constructor :: { Variable }
  : UPPER_ID {% flip mkVar (getText $1) `fmap` mkSpanPos (startPos $ span $1) }

ProgVarWild :: { Variable }
  : ProgVar { $1 }
  | '_'     {% flip mkVar "_" `fmap` mkSpanPos (startPos $ span $1) }

ProgVarWildSeq :: { [Variable] }
  :                            { [] }
  | ProgVarWild ProgVarWildSeq {% checkDupBind $1 $2 >> return ($1 : $2) }

ProgVarWildTBind :: { (Variable, T.Type) }
  : ProgVarWild ':' Type  %prec ProgVarWildTBind { ($1, $3) }

-- TYPE VARIABLE

TypeVar :: { Variable }
  : LOWER_ID {% flip mkVar (getText $1) `fmap` mkSpanPos (startPos $ span $1) }

TypeName :: { Variable }
  : UPPER_ID {% flip mkVar (getText $1) `fmap` mkSpanPos (startPos $ span $1) }

KindBind :: { (Variable, K.Kind) }
  : TypeVar ':' Kind { ($1, $3) }
  | TypeVar          { ($1, omission (span $1)) }

KindedTVar :: { (Variable, K.Kind) }    -- for type and data declarations
  : TypeName ':' Kind { ($1, $3) }
  | TypeName          { ($1, omission (span $1)) }

{

-- Span functions
-- TODO: move them to utils

mkSpanPos :: Pos -> FreestStateT Span
mkSpanPos p = do
  f <- getFileName
  maybe (Span p p f) (Span p p) <$> getModuleName
  
mkSpanPosPos :: Pos -> Pos -> FreestStateT Span
mkSpanPosPos p1 p2 = do
  f <- getFileName
  maybe (Span p1 p2 f) (Span p1 p2) <$> getModuleName

mkSpanFromSpan :: Span -> FreestStateT Span
mkSpanFromSpan (Span p1 p2 _) = do
  f <- getFileName
  maybe (Span p1 p2 f) (Span p1 p2) <$> getModuleName

-- Parse functions
-- Used in the Read instances
  
parse :: String -> FilePath -> ([Token] -> FreestStateT a) -> FreestStateT a
parse input fname parseFun = either (lift . Left) parseFun (scanTokens input fname)

parseKind :: FilePath -> String -> Either Errors K.Kind
parseKind runFilePath str = either (Left . (:[])) (Right . id) (evalStateT (parse str "" kinds) state)
  where
    state = initialState { runOpts = defaultOpts {runFilePath}}
--    state = initialState { runOpts = defaultOpts {runFilePath = "Parse.Kind"}}

parseType :: FilePath -> String -> Either Errors T.Type
parseType runFilePath str = either (Left . (:[])) stateToEither (runStateT (parse str "" types) state)
  where
    state = initialState { runOpts = defaultOpts {runFilePath}}
--     state = initialState { runOpts = defaultOpts {runFilePath = "Parse.Type"}}

parseExpr :: FilePath -> String -> Either Errors E.Exp
parseExpr runFilePath str = either (Left . (:[])) stateToEither (runStateT (parse str "" expr) state)
  where
    state = initialState { runOpts = defaultOpts {runFilePath}}

stateToEither :: (a, FreestS) -> Either Errors a
stateToEither (t,s)
  | hasErrors s = Left $ errors s
  | otherwise   = Right t


-- FreeST parsing functions 

-- Parses a the module header and then the program
parseProgram :: FreestS -> IO FreestS
parseProgram s = do
  let filename = runFilePath $ runOpts s
  input <- readFile filename
  let mh = parseModHeader s filename input
  return $ parseDefs (s {moduleName = moduleName mh}) filename input


parseModHeader :: FreestS -> FilePath -> String -> FreestS
parseModHeader s filename input =
  either (\e -> s { errors = [e] }) id (execStateT (parse input filename modname) s)

parseDefs :: FreestS -> FilePath -> String -> FreestS
parseDefs s filename input =
  either (\e -> s { errors = [e] }) id (execStateT (parse input filename terms) s)


parseAndImport :: FreestS -> IO FreestS
parseAndImport initial = do
  let filename = runFilePath $ runOpts initial 
  s <- parseProgram (initial {moduleName = Nothing})
  let baseName = takeBaseName (runFilePath $ runOpts s)
  case moduleName s of
    Just name
      | name == baseName -> doImports filename (Set.singleton name) (Set.toList (imports s)) s
      | otherwise -> pure $ s {errors = errors s ++ [NameModuleMismatch defaultSpan name baseName]}
    Nothing   -> doImports filename Set.empty (Set.toList (imports s)) s
  where
    doImports :: FilePath -> Imports -> [FilePath] -> FreestS -> IO FreestS
    doImports _ _ [] s = return s
    doImports defModule imported (curImport:toImport) s
      | curImport `Set.member` imported = doImports defModule imported toImport s
      | otherwise = do
          let fileToImport = replaceBaseName defModule curImport -<.> "fst"
          exists <- doesFileExist fileToImport
          if exists then do
            s' <- parseProgram (s {moduleName = Nothing, runOpts=defaultOpts{runFilePath=fileToImport}})
            let modName = fromJust $ moduleName s'
            if curImport /= modName then
              pure $ s' {errors = errors s ++ [NameModuleMismatch defaultSpan{defModule} modName curImport]}
            else
              doImports defModule (Set.insert curImport imported) (toImport ++ Set.toList (imports s')) s'
          else
            pure $ s {errors = errors s ++ [ImportNotFound defaultSpan{defModule} curImport fileToImport]}
        
-- Error Handling
parseError :: [Token] -> FreestStateT a
parseError [] = lift . Left $ PrematureEndOfFile defaultSpan
parseError (x:_) = lift . Left $ ParseError (span x) (show x)

}
