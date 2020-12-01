{
module Parse.Parser
-- ( parseType
-- , parseTypeScheme
-- , parseDefs
-- , parseProgram
-- , parseSchemes
--)
where


import           Syntax.Expression
import           Syntax.Schemes
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.ProgramVariables
import           Syntax.TypeVariables
import           Syntax.Base
import           Parse.ParseUtils
import           Parse.Lexer
import           Utils.Errors
import           Utils.FreestState
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Control.Monad.State
import           Data.Char
import           Data.List (nub, (\\), intercalate, find)
import           System.Exit (die)
import           Debug.Trace
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
-- Types
%right '=>'      -- Used in forall                 
%right '.'       -- used in rec
%right '->' '-o' -- an Expr operator as well
%right ';'       -- an Expr operator as well                
%right dualof

-- Lambda expressions
%nonassoc ProgVarWildTBind

%%

---------------
-- PROGRAM --
---------------

Prog :: { () }
  : Decl         {}
  | Decl NL Prog {}

NL :: { () }
  : nl NL {}
  | nl    {}

Decl :: { () }
  -- Function signature
--  : ProgVar ':' TypeScheme {% do
  : ProgVar ':' Type {% do
      toStateT $ checkDupProgVarDecl $1
      toStateT $ addToVEnv $1 $3 }
  -- Function declaration
  | ProgVar ProgVarWildSeq '=' Expr {% do
      toStateT $ checkDupFunDecl $1
      e <- toStateT $ buildFunBody $1 $2 $4
      toStateT $ addToEEnv $1 e }
  -- Type abbreviation
  | type KindedTVar KindBindEmptyList '=' Type {% do
      toStateT $ checkDupTypeDecl (fst $2)
      toStateT $ uncurry addToTEnv $2 $5 } -- TODO: add KindBindEmptyList ?
--      toStateT $ uncurry addToTEnv $2 (TypeScheme (pos $5) $3 $5) }
  -- Datatype declaration
  | data KindedTVar KindBindEmptyList '=' DataCons {% do
      let a = fst $2
      toStateT $ checkDupTypeDecl a
      let bs = typeListToType a $5 :: [(ProgVar, Type)]
      toStateT $ mapM_ (\(c, t) -> addToVEnv c t) bs
      let p = pos a
      toStateT $ uncurry addToTEnv $2 (Datatype p (Map.fromList bs))
    } -- TODO: add KindBindEmptyList ?
      -- toStateT $ uncurry addToTEnv $2 (TypeScheme p $3 (Datatype p (Map.fromList bs)))

DataCons :: { [(ProgVar, [Type])] }
  : DataCon              {% toStateT $ checkDupCons $1 [] >> return [$1] }
  | DataCon '|' DataCons {% toStateT $ checkDupCons $1 $3 >> return ($1 : $3) }

DataCon :: { (ProgVar, [Type]) }
  : Constructor TypeSeq { ($1, $2) }

-----------------
-- EXPRESSIONS --
-----------------

Expr :: { Exp }
  : let ProgVarWild '=' Expr in Expr { UnLet (pos $1) $2 $4 $6 }
  | Expr ';' Expr                    { App (pos $1)
                                         (Abs (pos $1) Un
                                           (TypeBind (pos $1) (mkVar (pos $1) "_")
                                             (UnitType (pos $3)))
                                           $3)
                                       $1}
  | let '(' ProgVarWild ',' ProgVarWild ')' '=' Expr in Expr
                                     { BinLet (pos $1) $3 $5 $8 $10 }
  | if Expr then Expr else Expr      { Conditional (pos $1) $2 $4 $6 }
  | new Type                         { New (pos $1) $2 (Dualof (negPos (pos $2)) $2) }
  | match Expr with '{' MatchMap '}' { Match (pos $1) $2 $5 }
  | case Expr of '{' CaseMap '}'     { Case (pos $1) $2 $5 }
  | Expr '$' Expr                    { App (pos $2) $1 $3 }
  | Expr '&' Expr                    { App (pos $2) $3 $1 }
  | Expr '||' Expr                   { binOp $1 (mkVar (pos $2) "(||)") $3 }
  | Expr '&&' Expr                   { binOp $1 (mkVar (pos $2) "(&&)") $3 }
  | Expr CMP Expr                    { binOp $1 (mkVar (pos $2) (getText $2)) $3 }
  | Expr '+' Expr                    { binOp $1 (mkVar (pos $2) "(+)") $3 }
  | Expr '-' Expr                    { binOp $1 (mkVar (pos $2) "(-)") $3 }
  | Expr '*' Expr                    { binOp $1 (mkVar (pos $2) "(*)") $3 }
  | Expr '/' Expr                    { binOp $1 (mkVar (pos $2) "div") $3 }
  
  | App                              { $1 }

App :: { Exp }
  : App Primary                      { App (pos $1) $1 $2 }
  | select ArbitraryProgVar          { Select (pos $1) $2 }
  | '-' App %prec NEG                { unOp (mkVar (pos $1) "negate") $2}
  | Primary                          { $1 }

Primary :: { Exp }
  : INT                                        { let (TokenInt p x) = $1 in Integer p x }
  | BOOL                                       { let (TokenBool p x) = $1 in Boolean p x }
  | CHAR                                       { let (TokenChar p x) = $1 in Character p x }
  | '()'                                       { Unit (pos $1) }
--  | ProgVar '[' Type ']'                     { TypeApp (pos $1) $1 $3 }
  | Primary '[' Type ']'                       { TypeApp (pos $1) $1 $3 }
  | ArbitraryProgVar                           { ProgVar (pos $1) $1 }
  | '(' lambda ProgVarWildTBind Arrow Expr ')' { Abs (pos $2) (snd $4)
                                                 (TypeBind (pos $2) (fst $3) (snd $3)) $5 }
  | '(' Lambda KindBind '=>' Expr ')'          { TypeAbs (pos $2) $3 $5 }    
  --| '(' Expr ',' Expr ')'                    { Pair (pos $1)$2 $4 }
  | '(' Expr ',' Tuple ')'                    { Pair (pos $1) $2 $4 }
  | '(' Expr ')'                               { $2 }

ProgVarWildTBind :: { (ProgVar, Type) }
  : ProgVarWild ':' Type  %prec ProgVarWildTBind { ($1, $3) }

Tuple :: { Exp }
  : Expr               { $1 }
  | Expr ',' Tuple     { Pair (pos $1) $1 $3 }

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

-----------
-- TYPE SCHEMES --
-----------

-- TypeScheme :: { TypeScheme }
--   : forall KindBindList '=>' Type { TypeScheme (pos $1) $2 $4 }
--   | Type                             { TypeScheme (pos $1) [] $1 }

-----------
-- TYPES --
-----------

Type :: { Type }
  -- Functional types
--  : BasicType                     { uncurry Basic $1 }
  : BasicType                     { $1 }
  | Type Arrow Type               { uncurry Fun $2 $1 $3 }
  | '(' Type ',' TupleType ')'    { PairType (pos $1) $2 $4 }
  -- Session types
  | Skip                          { Skip (pos $1) }
  | Type ';' Type                 { Semi (pos $2) $1 $3 }
  | Polarity BasicType            { uncurry Message $1 $2 }
  | ChoiceView '{' FieldList '}'  { uncurry Choice $1 $3 }
  | rec KindBind '.' Type         { Rec (pos $1) $2 $4 }
  -- Polymorphism
  | forall KindBind '=>' Type     { Forall (pos $1) $2 $4 }
  -- Functional or session
  | TypeVar                       { TypeVar (pos $1) $1 }
  -- Type operators
  | dualof Type                   { Dualof (pos $1) $2 }
  | TypeName                      { TypeVar (pos $1) $1 } -- TODO: remove this one lex
  | '(' Type ')'                  { $2 }

BasicType :: { Type }
  : Int  { IntType (pos $1) }
  | Char { CharType (pos $1) }
  | Bool { BoolType (pos $1) }
  | '()' { UnitType (pos $1) }

TupleType :: { Type }
  : Type                    { $1 }
  | Type ',' TupleType     { PairType (pos $1) $1 $3 }

Polarity :: { (Pos, Polarity) }
  : '?' { (pos $1, In) }
  | '!' { (pos $1, Out) }

Arrow :: { (Pos, Multiplicity) }
  : '->' { (pos $1, Un) }
  | '-o' { (pos $1, Lin) }

ChoiceView :: { (Pos, Polarity) }
  : '+' { (pos $1, Out) }
  | '&' { (pos $1, In) }

FieldList :: { TypeMap }
  : Field               { uncurry Map.singleton $1 }
  | Field ',' FieldList {% toStateT $ checkDupField (fst $1) $3 >>
                           return (uncurry Map.insert $1 $3) }

Field :: { (ProgVar, Type) }
  : ArbitraryProgVar ':' Type { ($1, $3) }

-----------
-- TYPE LISTS AND SEQUENCES --
-----------

-- TypeList :: { [Type] }
--   : Type              { [$1] }
--   | Type ',' TypeList { $1 : $3 }

TypeSeq :: { [Type] }
  :              { [] }
  | Type TypeSeq { $1 : $2 }

-----------
-- KINDS --
-----------

Kind :: { Kind }
  : SU             { kindSU (pos $1) }
  | SL             { kindSL (pos $1) }
  | TU             { kindTU (pos $1) }
  | TL             { kindTL (pos $1) }
  | MU             { kindMU (pos $1) }
  | ML             { kindML (pos $1) }
--  | Kind '->' Kind { KindArrow (pos $1) $1 $3 }
-- TODO: arrow

-- PROGRAM VARIABLES

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

-- TYPE VARIABLES

TypeVar :: { TypeVar }
  : LOWER_ID { mkVar (pos $1) (getText $1) }

TypeName :: { TypeVar }
  : UPPER_ID { mkVar (pos $1) (getText $1) }

KindBind :: { KindBind }
  : TypeVar ':' Kind { KindBind (pos $1) $1 $3 }
  | TypeVar          { KindBind (pos $1) $1 (omission (pos $1)) }

KindedTVar :: { (TypeVar, Kind) }    -- for type and data declarations
  : TypeName ':' Kind { ($1, $3) }
  | TypeName          { ($1, omission (pos $1)) }

KindBindList :: { [KindBind] }
  : KindBind                     { [$1] }
  | KindBind ',' KindBindList {% toStateT $ checkDupKindBind $1 $3 >> return ($1 : $3) }

KindBindEmptyList :: { [KindBind] }
  :                 { [] }
  | KindBindList { $1 }

-- TYPE SCHEMES

--------------------
-- SCHEMES PARSER --
--------------------
-- Schemes :: { (TypeScheme, TypeScheme) }
--   : TypeScheme NL TypeScheme { ($1, $3) }


{

-----------------------
-- Parsing functions --
-----------------------

-- KINDS  

parseKind :: String -> Kind
parseKind str =
  case evalStateT (parse str "" kinds) (initialState "") of
    Ok x -> x
    Failed err -> error err


parseType :: String -> Either Type String
parseType str =
  case runStateT (parse str "" types) (initialState "") of
    Ok p -> eitherTypeErr p
    Failed err -> Right $ err
  where
    eitherTypeErr (t, state)
      | hasErrors state = Right $ getErrors state
      | otherwise       = Left t


----------------------
-- PARSING SCHEMES  --
----------------------

-- parseSchemes :: String -> String -> Either (TypeScheme, TypeScheme) String
-- parseSchemes file str =
--   case runStateT (parse str file schemes) (initialState file) of
--     Ok (t,s) ->
--       if hasErrors s then Right (getErrors s) else Left t
--     Failed err -> Right err

-----------------------
-- PARSING PROGRAMS  --
-----------------------

parseProgram :: FilePath -> Map.Map ProgVar Type -> IO FreestS
parseProgram inputFile vEnv = do
  src <- readFile inputFile
  return $ parseDefs inputFile vEnv src

parseDefs :: FilePath -> VarEnv -> String -> FreestS
parseDefs file vEnv str =
  let s = initialState file in
  case execStateT (parse str file terms) (s {varEnv = vEnv}) of
    Ok s1 -> s1
    Failed err -> s {errors = (errors s) ++ [err]}


parse str file f = lexer str file f


lexer str file f =
  case scanTokens str file of
    Right err -> failM err
    Left x    -> f x

-------------------
-- Handle errors --
-------------------

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
