{
module Parse.Parser
where

import           Control.Monad.State
import qualified Data.Map.Strict               as Map
import           Parse.Lexer
import           Parse.ParseUtils
import           Syntax.Base
import           Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           Util.Error
import           Util.FreestState

}

%name types Type
%name terms Prog
%name kinds Kind
%name expr Exp
%tokentype { Token }
%error { parseError }
%monad { FreestStateT } { (>>=) } { return }

%token
  nl       {TokenNL _}
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
-------------
-- PROGRAM --
-------------

Prog :: { () }
  : Decl         {}
  | Decl NL Prog {}

NL :: { () }
  : nl NL {}
  | nl    {}

Decl :: { () }
  -- Function signature
  : ProgVar ':' Type {% do
      toStateT $ checkDupProgVarDecl $1
      toStateT $ addToVEnv $1 $3
    }
  -- Function declaration
  | ProgVar ProgVarWildSeq '=' Exp {% do
      toStateT $ checkDupFunDecl $1
      toStateT $ addToPEnv $1 $2 $4        
    }
  -- Type abbreviation
  | type KindedTVar TypeDecl {% do
      toStateT $ checkDupTypeDecl (fst $2)
      toStateT $ uncurry addToTEnv $2 $3
    }
  -- Datatype declaration
  | data KindedTVar '=' DataCons {% do
      let a = fst $2
      toStateT $ checkDupTypeDecl a
      let bs = typeListToType a $4 :: [(ProgVar, T.Type)]
      toStateT $ mapM_ (\(c, t) -> addToVEnv c t) bs
      let p = pos a
      toStateT $ uncurry addToTEnv $2 (T.Datatype p (Map.fromList bs))
    }

TypeDecl :: { T.Type }
  : '=' Type { $2 }
  | KindBind TypeDecl { let (a,k) = $1 in T.Forall (pos a) (K.Bind (pos k) a k $2) }

DataCons :: { [(ProgVar, [T.Type])] }
  : DataCon              {% toStateT $ checkDupCons $1 [] >> return [$1] }
  | DataCon '|' DataCons {% toStateT $ checkDupCons $1 $3 >> return ($1 : $3) }

DataCon :: { (ProgVar, [T.Type]) }
  : Constructor TypeSeq { ($1, $2) }

----------------
-- EXPRESSION --
----------------

Exp :: { E.Exp }
  : let ProgVarWild '=' Exp in Exp { E.UnLet (pos $1) $2 $4 $6 }
  | Exp ';' Exp                    { E.App (pos $1)
                                         (E.Abs (pos $1) 
                                           (E.Bind (pos $1) Un (mkVar (pos $1) "_")
                                             (T.Unit (pos $3))
                                           $3))
                                       $1}
  | let '(' ProgVarWild ',' ProgVarWild ')' '=' Exp in Exp
                                   { E.BinLet (pos $1) $3 $5 $8 $10 }
  | if Exp then Exp else Exp       { E.Cond (pos $1) $2 $4 $6 }
  | new Type                       { E.New (pos $1) $2 (T.Dualof (negPos (pos $2)) $2) }
  | match Exp with '{' MatchMap '}'{ E.Match (pos $1) $2 $5 }
  | case Exp of '{' CaseMap '}'    { E.Case (pos $1) $2 $5 }
  | Exp '$' Exp                    { E.App (pos $2) $1 $3 }
  | Exp '&' Exp                    { E.App (pos $2) $3 $1 }
  | Exp '||' Exp                    { binOp $1 (mkVar (pos $2) "(||)") $3 }
  | Exp '&&' Exp                    { binOp $1 (mkVar (pos $2) "(&&)") $3 }
  | Exp CMP Exp                     { binOp $1 (mkVar (pos $2) (getText $2)) $3 }
  | Exp '+' Exp                    { binOp $1 (mkVar (pos $2) "(+)") $3 }
  | Exp '-' Exp                    { binOp $1 (mkVar (pos $2) "(-)") $3 }
  | Exp '*' Exp                    { binOp $1 (mkVar (pos $2) "(*)") $3 }
  | Exp '/' Exp                    { binOp $1 (mkVar (pos $2) "(/)") $3 }
  | Exp '^' Exp                    { binOp $1 (mkVar (pos $2) "(^)") $3 }
  | '-' App %prec NEG              { unOp (mkVar (pos $1) "negate") $2}  
  | '(' Op Exp ')'                 { unOp $2 $3 } -- left section
  | '(' Exp Op ')'                 { unOp $3 $2 } -- right section
  | '(' Exp '-' ')'                { unOp (mkVar (pos $2) "(-)") $2 } -- right section (-) 
  | App                            { $1 }
  
App :: { E.Exp }
  : App Primary                    { E.App (pos $1) $1 $2 }
  | select ArbitraryProgVar        { E.Select (pos $1) $2 }
  | Primary                        { $1 }

Primary :: { E.Exp }
  : INT                            { let (TokenInt p x) = $1 in E.Int p x }
  | BOOL                           { let (TokenBool p x) = $1 in E.Bool p x }
  | CHAR                           { let (TokenChar p x) = $1 in E.Char p x }
  | STR                            { let (TokenString p x) = $1 in String p x }
  | '()'                           { E.Unit (pos $1) }
  | TApp ']'                       { $1 }
  | ArbitraryProgVar               { E.Var (pos $1) $1 }
  | lambda ProgVarWildTBind Abs
      { let ((p,m),e) = $3 in E.Abs p (E.Bind p m (fst $2) (snd $2) e) }
  | Lambda KindBind TAbs
      { let (a,k) = $2 in E.TypeAbs (pos a) (K.Bind (pos k) a k $3) }
  | '(' Exp ',' Tuple ')'          { E.Pair (pos $1) $2 $4 }
  | '(' Exp ')'                    { $2 }

Abs :: { ((Pos, Multiplicity), E.Exp) }
  : Arrow Exp { ($1, $2) }
  | ProgVarWildTBind Abs
      { let ((p,m),e) = $2 in ((p, m), E.Abs p (E.Bind p m (fst $1) (snd $1) e)) }

TAbs :: { E.Exp }
  : '=>' Exp { $2 }
  | KindBind TAbs
      { let (a,k) = $1 in E.TypeAbs (pos a) (K.Bind (pos k) a k $2) }

TApp :: { E.Exp }
  : Primary '[' Type { E.TypeApp (pos $1) $1 $3 }
  | TApp ',' Type        { E.TypeApp (pos $1) $1 $3 }

Tuple :: { E.Exp }
  : Exp           { $1 }
  | Exp ',' Tuple { E.Pair (pos $1) $1 $3 }

MatchMap :: { FieldMap }
  : Match              { uncurry Map.singleton $1 }
  | Match ',' MatchMap {% toStateT $ checkDupCase (fst $1) $3 >>
                          return (uncurry Map.insert $1 $3) }

Match :: { (ProgVar, E.Exp) }
  : ArbitraryProgVar ProgVarWild '->' Exp { ($1, toLamFun $4 [$2]) }

CaseMap :: { FieldMap }
  : Case             { uncurry Map.singleton $1 }
  | Case ',' CaseMap {% toStateT $ checkDupCase (fst $1) $3 >>
                        return (uncurry Map.insert $1 $3)
                     }

Case :: { (ProgVar, E.Exp) }
  : Constructor ProgVarWildSeq '->' Exp { ($1, toLamFun $4 $2) }

Op :: { ProgVar }
   : '||'   { mkVar (pos $1) "(||)"      }
   | '&&'  { mkVar (pos $1) "(&&)"       }
   | CMP   { mkVar (pos $1) (getText $1) }
   | '+'   { mkVar (pos $1) "(+)"        }  
   | '*'   { mkVar (pos $1) "(*)"        }
   | '/'   { mkVar (pos $1) "(/)"        }
   | '^'   { mkVar (pos $1) "(^)"        }


----------
-- TYPE --
----------

Type :: { T.Type }
  -- Functional types
  : Int                           { T.Int (pos $1) }
  | Char                          { T.Char (pos $1) }
  | Bool                          { T.Bool (pos $1) }
  | String                        { T.String (pos $1) }
  | '()'                          { T.Unit (pos $1) }
  | Type Arrow Type %prec ARROW   { uncurry T.Fun $2 $1 $3 }
  | '(' Type ',' TupleType ')'    { T.Pair (pos $1) $2 $4 }
  -- Session types
  | Skip                          { T.Skip (pos $1) }
  | Type ';' Type                 { T.Semi (pos $2) $1 $3 }
  | Polarity Type %prec MSG       { uncurry T.Message $1 $2 }
  | ChoiceView '{' FieldList '}'  { uncurry T.Choice $1 $3 }
  -- Polymorphism and recursion
  | rec KindBind '.' Type
      { let (a,k) = $2 in T.Rec (pos $1) (K.Bind (pos a) a k $4) }
  | forall KindBind Forall
      { let (a,k) = $2 in T.Forall (pos $1) (K.Bind (pos a) a k $3) }
  | TypeVar                       { T.Var (pos $1) $1 }
  -- Type operators
  | dualof Type                   { T.Dualof (pos $1) $2 }
  | TypeName                      { T.Var (pos $1) $1 } -- TODO: remove this one lex
  | '(' Type ')'                  { $2 }

Forall :: { T.Type }
  : '.' Type { $2 }
  | KindBind Forall 
      { let (a,k) = $1 in T.Forall (pos a) (K.Bind (pos k) a k $2) }

TupleType :: { T.Type }
  : Type               { $1 }
  | Type ',' TupleType { T.Pair (pos $1) $1 $3 }

Arrow :: { (Pos, Multiplicity) }
  : '->' { (pos $1, Un) }
  | '-o' { (pos $1, Lin) }

Polarity :: { (Pos, T.Polarity) }
  : '!' { (pos $1, T.Out) }
  | '?' { (pos $1, T.In) }

ChoiceView :: { (Pos, T.Polarity) }
  : '+' { (pos $1, T.Out) }
  | '&' { (pos $1, T.In) }

FieldList :: { T.TypeMap }
  : Field               { uncurry Map.singleton $1 }
  | Field ',' FieldList {% toStateT $ checkDupField (fst $1) $3 >>
                           return (uncurry Map.insert $1 $3) }

Field :: { (ProgVar, T.Type) }
  : ArbitraryProgVar ':' Type { ($1, $3) }

-- TYPE SEQUENCE

TypeSeq :: { [T.Type] }
  :              { [] }
  | Type TypeSeq { $1 : $2 }

----------
-- KIND --
----------

Kind :: { K.Kind }
  : SU { K.su (pos $1) }
  | SL { K.sl (pos $1) }
  | TU { K.tu (pos $1) }
  | TL { K.tl (pos $1) }
  | MU { K.mu (pos $1) }
  | ML { K.ml (pos $1) }
--  | Kind '->' Kind { KindArrow (pos $1) $1 $3 }

-- PROGRAM VARIABLE

ArbitraryProgVar :: { ProgVar }
 : ProgVar     { $1 }
 | Constructor { $1 }

ProgVar :: { ProgVar }
  : LOWER_ID { mkVar (pos $1) (getText $1) }

Constructor :: { ProgVar }
  : UPPER_ID { mkVar (pos $1) (getText $1) }

ProgVarWild :: { ProgVar }
  : ProgVar { $1 }
  | '_'     { mkVar (pos $1) "_" }

ProgVarWildSeq :: { [ProgVar] }
  :                            { [] }
  | ProgVarWild ProgVarWildSeq {% toStateT $ checkDupBind $1 $2 >> return ($1 : $2) }

ProgVarWildTBind :: { (ProgVar, T.Type) }
  : ProgVarWild ':' Type  %prec ProgVarWildTBind { ($1, $3) }

-- TYPE VARIABLE

TypeVar :: { TypeVar }
  : LOWER_ID { mkVar (pos $1) (getText $1) }

TypeName :: { TypeVar }
  : UPPER_ID { mkVar (pos $1) (getText $1) }

KindBind :: { (TypeVar, K.Kind) }
  : TypeVar ':' Kind { ($1, $3) }
  | TypeVar          { ($1, omission (pos $1)) }
  
KindedTVar :: { (TypeVar, K.Kind) }    -- for type and data declarations
  : TypeName ':' Kind { ($1, $3) }
  | TypeName          { ($1, omission (pos $1)) }

{
-- Parsing Kinds, Types and Programs

parseKind :: String -> K.Kind
parseKind str =
  case evalStateT (lexer str "" kinds) (initialState "") of
    Ok x -> x
    Failed err -> error err

parseType :: String -> Either T.Type String
parseType str =
  case runStateT (lexer str "" types) (initialState "") of
    Ok p -> eitherTypeErr p
    Failed err -> Right $ err
  where
    eitherTypeErr (t, state)
      | hasErrors state = Right $ getErrors state
      | otherwise       = Left t


-- parseProgram :: FilePath -> Map.Map ProgVar T.Type -> IO FreestS
-- parseProgram inputFile vEnv = do
--   prelude <- getDataFileName "Prelude.fst"
--   str <- readFile prelude
--   let (Ok s) = execStateT (lexer str prelude terms) ((initialState inputFile) {varEnv = vEnv})

--   src <- readFile inputFile
--   return $ parseDefs inputFile s src

-- parseDefs :: FilePath -> FreestS -> String -> FreestS
-- parseDefs file fState str =
--   let s = initialState file in
--   case execStateT (lexer str file terms) fState of
--     Ok s1 -> s1
--     Failed err -> s {errors = errors s ++ [err]}


parseProgram :: FilePath -> Map.Map ProgVar T.Type -> IO FreestS
parseProgram inputFile vEnv = -- do
  parseDefs inputFile vEnv <$> readFile inputFile

parseDefs :: FilePath -> VarEnv -> String -> FreestS
parseDefs file vEnv str =
  let s = initialState file in
  case execStateT (lexer str file terms) (s {varEnv = vEnv}) of
    Ok s1 -> s1
    Failed err -> s {errors = (errors s) ++ [err]}

    

lexer str file f =
  case scanTokens str file of
    Right err -> failM err
    Left x    -> f x

-- Error Handling

parseError :: [Token] -> FreestStateT a
parseError [] = do
  file <- toStateT getFileName
  failM $ formatErrorMessages Map.empty defaultPos file
          [Error "Parse error:", Error "\ESC[91mPremature end of file\ESC[0m"]
parseError (x:_) = do
  file <- toStateT getFileName
--  traceM $ show xs
  failM $ formatErrorMessages Map.empty p file
    [Error "Parse error on input", Error $ "\ESC[91m'" ++ show x ++ "'\ESC[0m"]
 where p = pos x

failM :: String -> FreestStateT a
failM = lift . Failed

toStateT = state . runState

}
