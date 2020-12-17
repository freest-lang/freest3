{
module Parse.Parser
-- ( parseType
-- , parseTypeScheme
-- , parseDefs
-- , parseProgram
-- , parseSchemes
--)
where


import           Control.Monad.State
import           Data.Char
import           Data.List                      ( nub
                                                , (\\)
                                                , intercalate
                                                , find
                                                )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Parse.Lexer
import           Parse.ParseUtils
import           Syntax.Base
import           Syntax.Expression             as E
import qualified Syntax.Kind                   as K
import           Syntax.Program
import           Syntax.ProgramVariable
import qualified Syntax.Type                   as T
import           Syntax.TypeVariable
import           System.Exit                    ( die )
import           Utils.Error
import           Utils.FreestState

  }

%name types Type
-- %name typeScheme TypeScheme
%name terms Prog
%name kinds Kind
%name expr Expr
-- %name schemes Schemes
%tokentype { Token }
%error { parseError }
-- %monad { ParseResult } { thenParseResult } { returnParseResult }
-- %monad { FreestState }
%monad { FreestStateT } { (>>=) } { return }


%token
  nl       {TokenNL _}
  Int      {TokenIntT _}
  Char     {TokenCharT _}
  Bool     {TokenBoolT _}
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

%nonassoc LOWER_ID UPPER_ID
%nonassoc '(' '['
%nonassoc '()'
%nonassoc '[' -- used in type app e[T]

-- Expr
%right in else match case
-- %left select
%nonassoc new
%right '$'       -- function call
%left '&'        -- function call
%left '||'       -- disjunction
%left '&&'       -- conjunction
%nonassoc CMP    -- comparison (relational and equality)
%left '+' '-'    -- aditive
%left '*' '/'    -- multiplicative
%left NEG not    -- unary

-- Type                               
%right '=>'      -- Used in forall                 
%right '.'       -- used in rec
%right ARROW
%right '->' '-o' -- an Expr operator as well
%right ';'       -- an Expr operator as well
%right POL        -- both '!' and '?'
%right dualof
%nonassoc ProgVarWildTBind

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
  | ProgVar ProgVarWildSeq '=' Expr {% do
      toStateT $ checkDupFunDecl $1
      toStateT $ addToPEnv $1 $2 $4        
    }
      -- e <- toStateT $ buildFunBody $1 $2 $4
      -- toStateT $ addToEEnv $1 e
  -- Type abbreviation
  | type KindedTVar KindBindEmptyList '=' Type {% do
      toStateT $ checkDupTypeDecl (fst $2)
      toStateT $ uncurry addToTEnv $2 $5 } -- TODO: add KindBindEmptyList ?
--      toStateT $ uncurry addToTEnv $2 (TypeScheme (pos $5) $3 $5) }
  -- Datatype declaration
  | data KindedTVar KindBindEmptyList '=' DataCons {% do
      let a = fst $2
      toStateT $ checkDupTypeDecl a
      let bs = typeListToType a $5 :: [(ProgVar, T.Type)]
      toStateT $ mapM_ (\(c, t) -> addToVEnv c t) bs
      let p = pos a
      toStateT $ uncurry addToTEnv $2 (T.Datatype p (Map.fromList bs))
    } -- TODO: add KindBindEmptyList ?
      -- toStateT $ uncurry addToTEnv $2 (TypeScheme p $3 (Datatype p (Map.fromList bs)))

DataCons :: { [(ProgVar, [T.Type])] }
  : DataCon              {% toStateT $ checkDupCons $1 [] >> return [$1] }
  | DataCon '|' DataCons {% toStateT $ checkDupCons $1 $3 >> return ($1 : $3) }

DataCon :: { (ProgVar, [T.Type]) }
  : Constructor TypeSeq { ($1, $2) }

----------------
-- EXPRESSION --
----------------

Expr :: { Exp }
  : let ProgVarWild '=' Expr in Expr { E.UnLet (pos $1) $2 $4 $6 }
  | Expr ';' Expr                    { E.App (pos $1)
                                         (E.Abs (pos $1) Un
                                           (T.Bind (pos $1) (mkVar (pos $1) "_")
                                             (T.Unit (pos $3)))
                                           $3)
                                       $1}
  | let '(' ProgVarWild ',' ProgVarWild ')' '=' Expr in Expr
                                     { E.BinLet (pos $1) $3 $5 $8 $10 }
  | if Expr then Expr else Expr      { E.Conditional (pos $1) $2 $4 $6 }
  | new Type                         { E.New (pos $1) $2 (T.Dualof (negPos (pos $2)) $2) }
  | match Expr with '{' MatchMap '}' { E.Match (pos $1) $2 $5 }
  | case Expr of '{' CaseMap '}'     { E.Case (pos $1) $2 $5 }
  | Expr '$' Expr                    { E.App (pos $2) $1 $3 }
  | Expr '&' Expr                    { E.App (pos $2) $3 $1 }
  | Expr '||' Expr                   { binOp $1 (mkVar (pos $2) "(||)") $3 }
  | Expr '&&' Expr                   { binOp $1 (mkVar (pos $2) "(&&)") $3 }
  | Expr CMP Expr                    { binOp $1 (mkVar (pos $2) (getText $2)) $3 }
  | Expr '+' Expr                    { binOp $1 (mkVar (pos $2) "(+)") $3 }
  | Expr '-' Expr                    { binOp $1 (mkVar (pos $2) "(-)") $3 }
  | Expr '*' Expr                    { binOp $1 (mkVar (pos $2) "(*)") $3 }
  | Expr '/' Expr                    { binOp $1 (mkVar (pos $2) "div") $3 }
  | '-' App %prec NEG                { unOp (mkVar (pos $1) "negate") $2}  
  | App                              { $1 }

App :: { Exp }
  : App Primary                      { E.App (pos $1) $1 $2 }
  | select ArbitraryProgVar          { E.Select (pos $1) $2 }
  | Primary                          { $1 }

Primary :: { Exp }
  : INT                              { let (TokenInt p x) = $1 in E.Int p x }
  | BOOL                             { let (TokenBool p x) = $1 in E.Bool p x }
  | CHAR                             { let (TokenChar p x) = $1 in E.Char p x }
  | '()'                             { E.Unit (pos $1) }
  | Primary '[' Type ']'             { E.TypeApp (pos $1) $1 $3 }
  | ArbitraryProgVar                 { E.Var (pos $1) $1 }
  | '(' lambda ProgVarWildTBind Arrow Expr ')' { E.Abs (pos $2) (snd $4)
                                                  (T.Bind (pos $2) (fst $3) (snd $3)) $5 }
  | '(' Lambda KindBind '=>' Expr ')'{ E.TypeAbs (pos $2) $3 $5 }    
  | '(' Expr ',' Tuple ')'           { E.Pair (pos $1) $2 $4 }
  | '(' Expr ')'                     { $2 }

ProgVarWildTBind :: { (ProgVar, T.Type) }
  : ProgVarWild ':' Type  %prec ProgVarWildTBind { ($1, $3) }

Tuple :: { Exp }
  : Expr               { $1 }
  | Expr ',' Tuple     { E.Pair (pos $1) $1 $3 }

MatchMap :: { FieldMap }
  : Match              { uncurry Map.singleton $1 }
  | Match ',' MatchMap {% toStateT $ checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }

Match :: { (ProgVar, ([ProgVar], Exp)) }
  : ArbitraryProgVar ProgVarWild '->' Expr { ($1, ([$2], $4)) }

CaseMap :: { FieldMap }
  : Case             { uncurry Map.singleton $1 }
  | Case ',' CaseMap {% toStateT $ checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }

Case :: { (ProgVar, ([ProgVar], Exp)) }
  : Constructor ProgVarWildSeq '->' Expr { ($1, ($2, $4)) }

----------
-- TYPE --
----------

Type :: { T.Type }
  -- Functional types
  : Int                           { T.Int (pos $1) }
  | Char                          { T.Char (pos $1) }
  | Bool                          { T.Bool (pos $1) }
  | '()'                          { T.Unit (pos $1) }
  | Type Arrow Type %prec ARROW   { uncurry T.Fun $2 $1 $3 }
  | '(' Type ',' TupleType ')'    { T.Pair (pos $1) $2 $4 }
  -- Session types
  | Skip                          { T.Skip (pos $1) }
  | Type ';' Type                 { T.Semi (pos $2) $1 $3 }
  | Polarity Type %prec POL       { uncurry T.Message $1 $2 }
  | ChoiceView '{' FieldList '}'  { uncurry T.Choice $1 $3 }
  -- Polymorphism and recursion
  | rec KindBind '.' Type         { T.Rec (pos $1) $2 $4 }
  | forall KindBind '=>' Type     { T.Forall (pos $1) $2 $4 }
  | TypeVar                       { T.Var (pos $1) $1 }
  -- Type operators
  | dualof Type                   { T.Dualof (pos $1) $2 }
  | TypeName                      { T.Var (pos $1) $1 } -- TODO: remove this one lex
  | '(' Type ')'                  { $2 }

TupleType :: { T.Type }
  : Type               { $1 }
  | Type ',' TupleType { T.Pair (pos $1) $1 $3 }

Polarity :: { (Pos, T.Polarity) }
  : '?' { (pos $1, T.In) }
  | '!' { (pos $1, T.Out) }

Arrow :: { (Pos, Multiplicity) }
  : '->' { (pos $1, Un) }
  | '-o' { (pos $1, Lin) }

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

-- TypeList :: { [T.Type] }
--   : Type              { [$1] }
--   | Type ',' TypeList { $1 : $3 }

-- KIND

Kind :: { K.Kind }
  : SU             { K.su (pos $1) }
  | SL             { K.sl (pos $1) }
  | TU             { K.tu (pos $1) }
  | TL             { K.tl (pos $1) }
  | MU             { K.mu (pos $1) }
  | ML             { K.ml (pos $1) }
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

-- TYPE VARIABLE

TypeVar :: { TypeVar }
  : LOWER_ID { mkVar (pos $1) (getText $1) }

TypeName :: { TypeVar }
  : UPPER_ID { mkVar (pos $1) (getText $1) }

KindBind :: { K.Bind }
  : TypeVar ':' Kind { K.Bind (pos $1) $1 $3 }
  | TypeVar          { K.Bind (pos $1) $1 (omission (pos $1)) }

KindedTVar :: { (TypeVar, K.Kind) }    -- for type and data declarations
  : TypeName ':' Kind { ($1, $3) }
  | TypeName          { ($1, omission (pos $1)) }

KindBindList :: { [K.Bind] }
  : KindBind                     { [$1] }
  | KindBind ',' KindBindList {% toStateT $ checkDupKindBind $1 $3 >> return ($1 : $3) }

KindBindEmptyList :: { [K.Bind] }
  :                 { [] }
  | KindBindList { $1 }

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

parseProgram :: FilePath -> Map.Map ProgVar T.Type -> IO FreestS
parseProgram inputFile vEnv = do
  src <- readFile inputFile
  return $ parseDefs inputFile vEnv src

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
