{
{-# LANGUAGE TupleSections, NamedFieldPuns #-}
module Parse.Parser
where

import           Parse.Lexer
import           Parse.ParseUtils
import           Syntax.Base
import           Syntax.MkName
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
import           System.Directory
import           System.FilePath
import           Paths_FreeST ( getDataFileName )
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
  Float    {TokenFloatT _}
  Char     {TokenCharT _}
  String   {TokenStringT _}
  '()'     {TokenUnit _}
  '->'     {TokenUnArrow _}
  '1->'     {TokenLinArrow _}
  lambda   {TokenLambda _}
  Lambda   {TokenUpperLambda _}
  '@'      {TokenAt _}
  Skip     {TokenSkip _}
  End      {TokenEnd _}
  '('      {TokenLParen _}
  ')'      {TokenRParen _}
  ','      {TokenComma _}
  '['      {TokenLBracket _}
  ']'      {TokenRBracket _}
  ':'      {TokenColon _}
  '::'      {TokenDoubleColon _}
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
  '|>'     {TokenPipeOp _}
  '+'      {TokenPlus _}
  '+.'     {TokenPlusF _}
  '-'      {TokenMinus _}
  '*'      {TokenTimes _}
  '^'      {TokenRaise _}
  '++'     {TokenAppend _}
  '_'      {TokenWild _}
  '$'      {TokenDollar _}
  '.'      {TokenDot _}
  CMP       {TokenCmp _ _}
  UPPER_ID {TokenUpperId _ _}
  LOWER_ID {TokenLowerId _ _}
  rec      {TokenRec _}
  US       {TokenUnS _}
  LS       {TokenLinS _}
  UT       {TokenUnT _}
  LT       {TokenLinT _}
  -- UM       {TokenUnM _}
  -- LM       {TokenLinM _}
  INT      {TokenInt _ _ }
  FLOAT    {TokenFloat _ _}
  CHAR     {TokenChar _ _}
  STR      {TokenString _ _}
  let      {TokenLet _}
  in       {TokenIn _}
  '='      {TokenEq _}
  data     {TokenData _}
  type     {TokenType _}
  '|'      {TokenPipe _}
  otherwise{TokenOtherwise _}
  if       {TokenIf _}
  then     {TokenThen _}
  else     {TokenElse _}
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
%right '.'       -- ∀ a:k . T and μ a:k . T
%right '=>' '->' '1->' ARROW -- λλ a:k => e,  x:T -> e, λ x:T 1-> e, T -> T and T 1-> T
%right ';'       -- T;T and e;e
%left '@'
%right '$'       -- function call
%left '|>'        -- function call
%left '||'       -- disjunction
%left '&&'       -- conjunction
%left '++'
%nonassoc CMP    -- comparison (relational and equality)
%right '::'      -- lists
%left '+' '-' '+.'  -- aditive
%left '*' '/'    -- multiplicative
%right '^'       -- power
%left NEG not    -- unary
%right MSG       -- !T and ?T
%right dualof
%nonassoc ProgVarWildTBind


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
  :     ProgVarList ':' Type {% forM_ $1 (\x -> checkDupProgVarDecl x >> addToVEnv x $3) }
  -- |     ProgVarList ':' Type {% forM_ $1 (\x -> checkDupProgVarDecl x >> addToVEnv x $3) }
  -- Function declaration
  | ProgVar PatternSeq '=' Exp   {% addToPEnvPat $1 $2 $4 }
  | ProgVar PatternSeq GuardsFun {% addToPEnvPat $1 $2 $3 }
  -- Type abbreviation
  | type KindedTVar TypeDecl {% checkDupTypeDecl (fst $2) >> uncurry addToTEnv $2 $3 }
  -- Datatype declaration
  | data KindedTVar '=' DataCons {% do
      let a = fst $2
      checkDupTypeDecl a
      mapM_ (uncurry addToVEnv) (typeListsToUnArrows a $4) -- fixed in elaboration
      uncurry addToTEnv $2 (T.Labelled (getSpan a) T.Variant (typeListToRcdType $4))
    }

ProgVarList :: { [Variable] }
  : ProgVar                 { [$1] }
  | ProgVar ',' ProgVarList { $1 : $3}

TypeDecl :: { T.Type }
  : '=' Type { $2 }
  | KindBind TypeDecl { let (a,k) = $1 in T.Forall (getSpan a) (Bind (getSpan k) a k $2) }

DataCons :: { [(Variable, [T.Type])] }
  : DataCon              {% checkDupCons $1 [] >> return [$1] }
  | DataCon '|' DataCons {% checkDupCons $1 $3 >> return ($1 : $3) }
  
DataCon :: { (Variable, [T.Type]) }
  : Constructor TypeSeq { ($1, $2) }

----------------
-- EXPRESSION --
----------------

Exp :: { E.Exp }
  : let ProgVarWild '=' Exp in Exp {% mkSpanSpan $1 $6 >>= \s -> pure $ E.UnLet s $2 $4 $6 }
  | Exp ';' Exp                    {% mkSpanSpan $1 $3 >>= \s -> pure $ E.UnLet s (mkWild s) $1 $3 }
  | let '(' ProgVarWild ',' ProgVarWild ')' '=' Exp in Exp
                                   {% mkSpanSpan $1 $10 >>= \s -> pure $ E.BinLet s $3 $5 $8 $10 }
  | if Exp then Exp else Exp       {% mkSpanSpan $1 $6 >>= \s -> pure $ condCase s $2 $4 $6}
  | match Exp with '{' MatchMap '}' {% let s' = getSpan $2 in mkSpanSpan $1 $6 >>= \s ->
                                       pure $ E.Case s (E.App s' (E.Var s' (mkCollect s')) $2) $5 }
  | case Exp of '{' CaseMap '}'    {% mkSpanSpan $1 $6 >>= \s -> pure $ E.CasePat s $2 $5 }
  | Exp '$' Exp                    {% mkSpanSpan $1 $3 >>= \s -> pure $ E.App s $1 $3 }
  | Exp '|>' Exp                    {% mkSpanSpan $1 $3 >>= \s -> pure $  E.App s $3 $1 }
  | Exp '||' Exp                   {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkOr s) $3 }
  | Exp '&&' Exp                   {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkAnd s) $3 }
  | Exp CMP Exp                    {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkVar s (getText $2)) $3 }
  | Exp '+' Exp                    {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkPlus s) $3 }
  | Exp '+.' Exp                   {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkVar s "(+.)") $3}
  | Exp '-' Exp                    {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkMinus s) $3 }
  | Exp '*' Exp                    {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkTimes s) $3 }
  | Exp '/' Exp                    {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkDiv s) $3 }
  | Exp '^' Exp                    {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkPower s) $3 }
  | Exp '++' Exp                   {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkVar s "(++)") $3 } -- TODO:  mkFun on MkName.hs
  | Exp '::' Exp                   {% mkSpan $2 >>= \s -> pure $ binOp $1 (mkCons s) $3 }
  | '-' App %prec NEG              {% mkSpan $1 >>= \s -> pure $ unOp (mkNeg s) $2 s }
  | App                            { $1 }

App :: { E.Exp }
  : App Primary                    {% mkSpanSpan $1 $2 >>= \s -> return $ E.App s $1 $2 }
  | select Constructor             {% mkSpanSpan $1 $2 >>= \s -> mkSpan $2 >>=
                                       \s1 -> pure $ E.App s (E.Var s (mkSelect s1)) (E.Var s1 $2)
                                   }
  | Primary                        { $1 }
  | App '@' Type                   {% mkSpanSpan $1 $3 >>= \s -> pure $ E.TypeApp s $1 $3 }
   
Primary :: { E.Exp }
  : INT                            {% let (TokenInt p x) = $1 in flip E.Int x `fmap` liftModToSpan p }
  | FLOAT                          {% let (TokenFloat p x) = $1 in flip E.Float x `fmap` liftModToSpan p}
  | CHAR                           {% let (TokenChar p x) = $1 in flip E.Char x `fmap` liftModToSpan p }
  | STR                            {% let (TokenString p x) = $1 in flip String x `fmap` liftModToSpan p }
  | '()'                           {% E.Unit `fmap` mkSpan $1 }
  | '[' Exp ExpList                { binOp $2 (mkCons (getSpan $2)) $3 }
  | '['']'                         {% mkSpan $1 >>= \s -> pure $ E.Var s (mkNil s) }
  | ArbitraryProgVar               {% flip E.Var $1 `fmap` mkSpan $1 }
  | lambda ProgVarWildTBind Abs    {% let (m,e) = $3 in mkSpanSpan $1 e >>= \s -> pure $ E.Abs s m (Bind s (fst $2) (snd $2) e) }
  | Lambda KindBind TAbs           {% let (a,k) = $2 in mkSpanSpan $1 $3 >>= \s -> pure $ E.TypeAbs s (Bind s a k $3) }
  | '(' Op Exp ')'                 {% mkSpanSpan $1 $4 >>= leftSection $2 $3 } -- left section
  | '(' Exp Op ')'                 {% mkSpanSpan $1 $4 >>= pure . unOp $3 $2 } -- right section
  | '(' Exp '-' ')'                {% mkSpan $2 >>= \s -> pure $ unOp (mkMinus s) $2 s } -- right section (-)
  | '(' Exp ',' Tuple ')'          {% mkSpanSpan $1 $5 >>= \s -> pure $ E.Pair s $2 $4 }
  | '(' Exp ')'                    { $2 }


Abs :: { (Multiplicity, E.Exp) }
  : Arrow Exp { ($1, $2) }
  | ProgVarWildTBind Abs
      {% let (v, t) = $1 in
         let (m, e) = $2 in
         mkSpanSpan v e >>= \s -> pure (m, E.Abs s m (Bind s v t e))
      }

TAbs :: { E.Exp }
  : '=>' Exp { $2 }
  | KindBind TAbs
      {% let (a,k) = $1 in mkSpanSpan a $2 >>=
         \s -> mkSpanSpan k $2 >>=
         \s' -> pure $ E.TypeAbs s (Bind s' a k $2)
      }

Tuple :: { E.Exp }
  : Exp           { $1 }
  | Exp ',' Tuple {% mkSpanSpan $1 $3 >>= \s -> pure $ E.Pair s $1 $3 }
--  | Exp ',' Tuple {% mkSpanPosPos (startPos $ getSpan $1) (endPos $ getSpan $3) >>= \s -> pure $ E.Pair s $1 $3 }

MatchMap :: { FieldMap }
  : Match              { uncurry Map.singleton $1 }
  | Match ',' MatchMap {% checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }

Match :: { (Variable, ([Variable], E.Exp)) }
  : ArbitraryProgVar ProgVarWild '->' Exp { ($1, ([$2], $4)) }

CaseMap :: { FieldList }
  : Case             { [$1] }
  | Case ',' CaseMap { $1 : $3 }

Case :: { ([Pattern], E.Exp) }
  : PatternCase '->' Exp   { ([$1],$3) }
  | PatternCase GuardsCase { ([$1],$2) }

PatternSeq :: { [Pattern] }
  :                    { [] }
  | Pattern PatternSeq {% checkDupVarPats ($1:$2) >> return ($1:$2) }

PatternCase :: { Pattern }
  : Constructor Pattern PatternSeq {% checkDupVarPats ($2:$3) >> return (E.PatCons $1 ($2:$3)) }
  | Pattern    { $1 }

Pattern :: { Pattern }
  : ProgVarWild                            { E.PatVar  $1    }
  | Constructor                            { E.PatCons $1 [] }
  | '['']'                                 { E.PatCons (mkNil $ getSpan $1) [] }
  | '(' Pattern '::' Pattern ')'           { E.PatCons (mkCons $ getSpan $3) ($2:[$4]) }
  | '(' Constructor Pattern PatternSeq ')' { E.PatCons $2 ($3:$4) }
  | '(' Pattern ')'                        { $2 }

GuardsCase :: { Exp }
  : '|' Exp       '->' Exp GuardsCase {% mkSpanSpan $1 $4 >>= \s -> pure $ condCase s $2 $4 $5 }
  | '|' otherwise '->' Exp            { $4 }

GuardsFun :: { Exp }
  : '|' Exp       '=' Exp GuardsFun   {% mkSpanSpan $1 $4 >>= \s -> pure $ condCase s $2 $4 $5 }
  | '|' otherwise '=' Exp             { $4 }


ExpList :: { E.Exp }
  : ']'             { E.Var (getSpan $1) (mkNil (getSpan $1)) }
  | ',' Exp ExpList { binOp $2 (mkCons (getSpan $2)) $3 }


Op :: { Variable }
   : '||'  {% mkOr `fmap` mkSpan $1 }
   | '&&'  {% mkAnd `fmap` mkSpan $1  }
   | CMP   {% flip mkVar (getText $1) `fmap` mkSpan $1 }
   | '+'   {% mkPlus `fmap` mkSpan $1  }
   | '+.'  {% flip mkVar "(+.)" `fmap` mkSpan $1}
   | '*'   {% mkTimes `fmap` mkSpan $1  }
   | '/'   {% mkDiv `fmap` mkSpan $1 }
   | '^'   {% mkPower `fmap` mkSpan $1 }
   | '++'  {% flip mkVar "(++)" `fmap` mkSpan $1 }
   | '|>'  {% flip mkVar "(|>)" `fmap` mkSpan $1 }
   | '$'   {% flip mkVar "($)" `fmap` mkSpan $1 }
-- TODO: add List funs

----------
-- TYPE --
----------

Type :: { T.Type }
  -- Functional types
  : Int                           {% T.Int `fmap` mkSpan $1 }
  | Float                         {% T.Float `fmap` mkSpan $1}
  | Char                          {% T.Char `fmap` mkSpan $1 }
  | String                        {% T.String `fmap` mkSpan $1 }
  | '()'                          {% mkSpan $1 >>= \s -> pure $ T.unit s}
  | Type Arrow Type %prec ARROW   {% mkSpanSpan $1 $3 >>= \s -> pure $ T.Arrow s $2 $1 $3 }
  | '(' Type ',' TupleType ')'    {% mkSpanSpan $1 $5 >>= \s -> pure $ T.tuple s [$2,$4]}
  | '[' Int ']'                   {% mkSpanSpan $1 $3 >>= \s -> pure $ T.Var s $ mkList s }
  -- Session types
  | Skip                          {% T.Skip `fmap` mkSpan $1 }
  | End                           {% T.End `fmap` mkSpan $1 }
  | Type ';' Type                 {% mkSpanSpan $1 $3 >>= \s -> pure $ T.Semi s $1 $3 }
  | Polarity Type %prec MSG       {% mkSpanFromSpan (fst $1) $2 >>= \s -> pure $ T.Message s (snd $1) $2 }                                 
  | ChoiceView '{' FieldList '}'  {% addToPEnvChoices (Map.keys $3)
                                  >> mkSpanFromSpan (fst $1) $4 >>= \s -> pure $ T.Labelled s (T.Choice (snd $1)) $3 } 
  -- Star types
  | '*' Polarity Type %prec MSG 
    {% do
        p <- mkSpan $1
        tVar <- freshTVar "a" p
        return (T.Rec p $ Bind p tVar (K.us p) $
          T.Semi p (uncurry T.Message $2 $3) (T.Var p tVar)) }
  | '*' ChoiceView '{' LabelList '}'
    {% do
        p <- mkSpan $1
        tVar <- freshTVar "a" p
        let tMap = Map.map ($ (T.Var p tVar)) $4
        return (T.Rec p $ Bind p tVar (K.us p) $
            T.Labelled (fst $2) (T.Choice (snd $2)) tMap) }

  -- Polymorphism and recursion
  | rec KindBind '.' Type         {% let (a,k) = $2 in flip T.Rec (Bind (getSpan a) a k $4) `fmap` mkSpanSpan $1 $4 }
  | forall KindBind Forall        {% let (a,k) = $2 in flip T.Forall (Bind (getSpan a) a k $3) `fmap` mkSpanSpan $1 $3 }
  | TypeVar                       {% flip T.Var $1 `fmap` mkSpan $1 }
  -- Type operators
  | dualof Type                   {% flip T.Dualof $2 `fmap` mkSpanSpan $1 $2 }
  | TypeName                      {% flip T.Var $1 `fmap` mkSpan $1 }   -- TODO: remove this one lex
  | '(' Type ')'                  { $2 }

Forall :: { T.Type }
  : '.' Type { $2 }
  | KindBind Forall
      { let (a,k) = $1 in T.Forall (getSpan a) (Bind (getSpan k) a k $2) }

TupleType :: { T.Type }
  : Type               { $1 }
  | Type ',' TupleType { T.tuple (getSpan $1) [$1,$3] }
                                               

Arrow :: { Multiplicity }
  : '->' { Un  }
  | '1->' { Lin }

Polarity :: { (Span, T.Polarity) }
  : '!' { (getSpan $1, T.Out) }
  | '?' { (getSpan $1, T.In) }

ChoiceView :: { (Span, T.View) }
  : '+' { (getSpan $1, T.Internal) }
  | '&' { (getSpan $1, T.External) }

FieldList :: { T.TypeMap }
  : Field               { uncurry Map.singleton $1 }
  | Field ',' FieldList {% checkDupField (fst $1) $3 >>
                           return (uncurry Map.insert $1 $3) }
                           
Field :: { (Variable, T.Type) }
  : ArbitraryProgVar ':' Type { ($1, $3) }

LabelList :: { Map.Map Variable (T.Type -> T.Type) }
  : ArbitraryProgVar               { Map.singleton $1 id }
  | ArbitraryProgVar ',' LabelList {% checkDupField $1 $3 >>
                                    return (Map.insert $1 id $3) }

-- TYPE SEQUENCE

TypeSeq :: { [T.Type] }
  :              { [] }
  | Type TypeSeq { $1 : $2 }

----------
-- KIND --
----------

Kind :: { K.Kind }
  : US {% K.us `fmap` mkSpan $1 }
  | LS {% K.ls `fmap` mkSpan $1 }
  | UT {% K.ut `fmap` mkSpan $1 }
  | LT {% K.lt `fmap` mkSpan $1 }
  -- | UM {% K.um `fmap` mkSpan $1 }
  -- | LM {% K.lm `fmap` mkSpan $1 }

-- PROGRAM VARIABLE

ArbitraryProgVar :: { Variable }
 : ProgVar     { $1 }
 | Constructor { $1 }

ProgVar :: { Variable }
  : LOWER_ID    {% flip mkVar (getText $1) `fmap` mkSpan $1 }
  | '(' Op ')'  {% mkSpanSpan $1 $3 >>= \s -> pure $ mkVar s $ intern $2 }
  | '(' '-' ')' {% mkSpanSpan $1 $3 >>= \s -> pure $ mkMinus s }

Constructor :: { Variable }
  : UPPER_ID {% flip mkVar (getText $1) `fmap` mkSpan $1 }

ProgVarWild :: { Variable }
  : ProgVar { $1 }
  | '_'     {% mkWild `fmap` mkSpan $1 }

-- TODO remove, only used on case
-- ProgVarWildSeq :: { [Variable] }
--   :                            { [] }
--   | ProgVarWild ProgVarWildSeq {% checkDupBind $1 $2 >> return ($1 : $2) }

ProgVarWildTBind :: { (Variable, T.Type) }
  : ProgVarWild ':' Type  %prec ProgVarWildTBind { ($1, $3) }

-- TYPE VARIABLE

TypeVar :: { Variable }
  : LOWER_ID {% flip mkVar (getText $1) `fmap` mkSpan $1 }

TypeName :: { Variable }
  : UPPER_ID {% flip mkVar (getText $1) `fmap` mkSpan $1 }

KindBind :: { (Variable, K.Kind) }
  : TypeVar ':' Kind { ($1, $3) }
  | TypeVar          { ($1, omission (getSpan $1)) }

KindedTVar :: { (Variable, K.Kind) }    -- for type and data declarations
  : TypeName ':' Kind { ($1, $3) }
  | TypeName          { ($1, omission (getSpan $1)) }

{

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
          if exists then
            importModule s fileToImport curImport defModule imported toImport
          else do
            fileToImport <- getDataFileName $ curImport -<.> "fst"
            isStdLib <- doesFileExist fileToImport
            if isStdLib then
              importModule s fileToImport curImport defModule imported toImport                
            else 
              pure $ s {errors = errors s ++ [ImportNotFound defaultSpan{defModule} curImport fileToImport]}

    importModule :: FreestS -> FilePath -> FilePath -> FilePath -> Imports -> [FilePath] -> IO FreestS
    importModule s fileToImport curImport defModule imported toImport = do
      s' <- parseProgram (s {moduleName = Nothing, runOpts=defaultOpts{runFilePath=fileToImport}})         
      let modName = fromJust $ moduleName s'
      if curImport /= modName then
        pure $ s' {errors = errors s ++ [NameModuleMismatch defaultSpan{defModule} modName curImport]}
      else
        doImports defModule (Set.insert curImport imported) (toImport ++ Set.toList (imports s')) s'

-- TODO: test MissingModHeader

{-      case moduleName s' of
        Just modName -> 
          if curImport /= modName then
            pure $ s' {errors = errors s ++ [NameModuleMismatch defaultSpan{defModule} modName curImport]}
          else
            doImports defModule (Set.insert curImport imported) (toImport ++ Set.toList (imports s')) s'
        Nothing ->
            pure $ s' {errors = errors s ++ [MissingModHeader defaultSpan{defModule} curImport]}
-}   

          
-- Error Handling
parseError :: [Token] -> FreestStateT a
parseError [] = lift . Left $ PrematureEndOfFile defaultSpan
parseError (x:_) = lift . Left $ ParseError (getSpan x) (show x)

}
