{
module Parse.Parser
( parseType
, parseTypeScheme
, parseDefs
, parseProgram
) where
  
import           Syntax.Expressions
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
-- import qualified Data.Set as Set
import           Control.Monad.State
import           Data.Char
import           Data.List (nub, (\\), intercalate, find)
import           System.Exit (die)
import           Debug.Trace
}

%name types Type
%name typeScheme TypeScheme
%name terms Prog
%name kinds Kind
%name expr Expr
%tokentype { Token }
%error { parseError }
%monad { FreestState }

%token
  nl      {TokenNL _}
  Int      {TokenIntT _}
  Char     {TokenCharT _}
  Bool     {TokenBoolT _}
  '()'     {TokenUnit _}
  '->'     {TokenUnArrow _}
  '-o'     {TokenLinArrow _}
  '\\'     {TokenLambda _}
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
  '&'      {TokenAmpersand _} 
  '+'      {TokenPlus _}
  '-'      {TokenMinus _}
  '*'      {TokenTimes _}
  '_'      {TokenWild _}
  OP       {TokenOp _ _}
  UPPER_ID {TokenUpperId _ _}
  LOWER_ID {TokenLowerId _ _}
  rec      {TokenRec _}
  '.'      {TokenDot _}
  SU       {TokenSU _}
  SL       {TokenSL _}
  TU       {TokenTU _}
  TL       {TokenTL _}
  INT      {TokenInteger _ _ }
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
  send     {TokenSend _}
  receive  {TokenReceive _}
  select   {TokenSelect _}
  match    {TokenMatch _}
  with     {TokenWith _}
  fork     {TokenFork _}
  case     {TokenCase _}
  of       {TokenOf _}
  forall   {TokenForall _}
  dualof   {TokenDualof _}  

-- Expr
%right in else match case
%left send receive select
%nonassoc new
%left '||'                      -- disjunction
%left '&&'                      -- conjunction
%left '==' '/='                 -- equality
%nonassoc OP -- '<' '<=' '>' '>='     -- relational
%left '+' '-'                   -- aditive
%left '*' '/'                   -- multiplicative
%left NEG not                   -- unary

-- Type
%right dualof
%right '->' '-o' -- TODO: an Expr operator as well
%right '.'       -- Used in rec
%right ';'       -- TODO: an Expr operator as well

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
  : ProgVar ':' TypeScheme {% do
      checkDupProgVarDecl $1
      addToVEnv $1 $3 }
  -- Function declaration
  | ProgVar ProgVarWildSeq '=' Expr {% do
      checkDupFunDecl $1
      e <- buildFunBody $1 $2 $4
      addToEEnv $1 e }
  -- Type abbreviation
  | type TypeNameKind TypeVarBindEmptyList '=' Type {% do
      checkDupTypeDecl (fst $2)
      uncurry addToTEnv $2 (TypeScheme (position $5) $3 $5) }
  -- Datatype declaration
  | data TypeNameKind TypeVarBindEmptyList '=' DataCons {% do
      let a = fst $2
      let p = position a
      checkDupTypeDecl a
      let bs = typeListToType a $5 :: [(ProgVar, Type)]
      mapM_ (\(c, t) -> addToVEnv c (toTypeScheme t)) bs
      vEnv <- getVEnv
      uncurry addToTEnv $2 (TypeScheme p $3 (Datatype p (Map.fromList bs)))
    }

DataCons :: { [(ProgVar, [Type])] }
  : DataCon              { [$1] }
  | DataCon '|' DataCons {% checkDupCons $1 $3 >> return ($1 : $3) }

DataCon :: { (ProgVar, [Type]) }
  : Constructor TypeSeq { ($1, $2) }

-----------------
-- EXPRESSIONS --
-----------------

Expr :: { Expression }
  : let ProgVarWild '=' Expr in Expr         { UnLet (position $1) $2 $4 $6 }
  | let ProgVarWild ',' ProgVarWild '=' Expr in Expr { BinLet (position $1) $2 $4 $6 $8 }
  | if Expr then Expr else Expr              { Conditional (position $1) $2 $4 $6 }
  | new Type                                 { New (position $1) $2 }
  | match Expr with '{' MatchMap '}'         { Match (position $1) $2 $5 }
  | case Expr of '{' CaseMap '}'             { Case (position $1) $2 $5 }
  | Expr '*' Expr                            { binOp $1 (mkVar (position $2) "(*)") $3 }
  | Expr '+' Expr                            { binOp $1 (mkVar (position $2) "(+)") $3 }
  | Expr '-' Expr                            { binOp $1 (mkVar (position $2) "(-)") $3 }
  | Expr OP Expr                             { binOp $1 (mkVar (position $2) (getText $2)) $3 }
  | App                                      { $1 }

App :: { Expression }
  : App Primary                              { App (position $1) $1 $2 }
  | ProgVar '[' TypeList ']'                 { TypeApp (position $1) $1 $3 }
  | send Primary                             { Send (position $1) $2 }
  | receive Primary                          { Receive (position $1) $2 }
  | select Constructor Primary               { Select (position $1) $2 $3 }
  | fork Primary                             { Fork (position $1) $2 }
  | '-' App %prec NEG                        { unOp (mkVar (position $1) "negate") $2}
  | Primary                                  { $1 }

Primary :: { Expression }
  : INT                                      { let (TokenInteger p x) = $1 in Integer p x }
  | BOOL                                     { let (TokenBool p x) = $1 in Boolean p x }
  | CHAR                                     { let (TokenChar p x) = $1 in Character p x }
  | '()'                                     { Unit (position $1) }
  | ProgVar                                  { ProgVar (position $1) $1 }
  | Constructor                              { ProgVar (position $1) $1 }
--  | '(' '\\' ProgVarWild ':' Type Arrow Expr ')' { Lambda (position $2) (snd $6) $3 $5 $7 }
  | '(' Expr ',' Expr ')'                    { Pair (position $1) $2 $4 }
  | '(' Expr ')'                             { $2 }

MatchMap :: { FieldMap }
  : Match              { uncurry Map.singleton $1 }
  | Match ';' MatchMap {% checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }

Match :: { (ProgVar, ([ProgVar], Expression)) }
  : Constructor ProgVarWild '->' Expr { ($1, ([$2], $4)) }

CaseMap :: { FieldMap }
  : Case             { uncurry Map.singleton $1 }
  | Case ';' CaseMap {% checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }
                        
Case :: { (ProgVar, ([ProgVar], Expression)) }
  : Constructor ProgVarWildSeq '->' Expr { ($1, ($2, $4)) }

-----------
-- TYPE SCHEMES --
-----------

TypeScheme :: { TypeScheme }
  : forall TypeVarBindList '=>' Type { TypeScheme (position $1) $2 $4 }
  | Type                             { TypeScheme (position $1) [] $1 }

-----------
-- TYPES --
-----------

Type :: { Type }
  -- Functional types
  : BasicType                    { uncurry Basic $1 }
  | Type Arrow Type              { uncurry Fun $2 $1 $3 }
  | '(' Type ',' Type ')'        { PairType (position $1) $2 $4 }
--  | '[' FieldList ']'            { Datatype (position $1) $2 }
  -- Session types
  | Skip                         { Skip (position $1) }
  | Type ';' Type                { Semi (position $2) $1 $3 }
  | Polarity BasicType           { uncurry Message $1 (snd $2) }
  | ChoiceView '{' FieldList '}' { uncurry Choice $1 $3 } 
  | rec TypeVarBind '.' Type     { Rec (position $1) $2 $4 }
  -- Functional or session
  | TypeVar                      { TypeVar (position $1) $1 }
  -- Type operators
  | dualof Type                  { Dualof (position $1) $2 }
  | TypeName                     { TypeName (position $1) $1 }
  | '(' Type ')'                 { $2 }

BasicType :: { (Pos, BasicType) }
  : Int  { (position $1, IntType) }
  | Char { (position $1, CharType) }
  | Bool { (position $1, BoolType) }
  | '()' { (position $1, UnitType) }

Polarity :: { (Pos, Polarity) }
  : '?' { (position $1, In) }
  | '!' { (position $1, Out) }

Arrow :: { (Pos, Multiplicity) }
  : '->' { (position $1, Un) }
  | '-o' { (position $1, Lin) }

ChoiceView :: { (Pos, Polarity) }
  : '+' { (position $1, Out) }
  | '&' { (position $1, In) }

FieldList :: { TypeMap }
  : Field               { uncurry Map.singleton $1 }
  | Field ',' FieldList {% checkDupField (fst $1) $3 >>
                           return (uncurry Map.insert $1 $3) }

Field :: { (ProgVar, Type) }
  : Constructor ':' Type { ($1, $3) }

-----------
-- TYPE LISTS AND SEQUENCES --
-----------

TypeList :: { [Type] }
  : Type              { [$1] }
  | Type ',' TypeList { $1 : $3 }

TypeSeq :: { [Type] }
  :              { [] }
  | Type TypeSeq { $1 : $2 }

-----------
-- KINDS --
-----------

Kind :: { Kind } :
    SU { Kind (position $1) Session Un }
  | SL { Kind (position $1) Session Lin }
  | TU { Kind (position $1) Functional Un }
  | TL { Kind (position $1) Functional Lin }

-- PROGRAM VARIABLES

ProgVar :: { ProgVar }
  : LOWER_ID { mkVar (position $1) (getText $1) }

Constructor :: { ProgVar }
  : UPPER_ID { mkVar (position $1) (getText $1) }

ProgVarWild :: { ProgVar }
  : ProgVar { $1 }
  | '_'     { mkVar (position $1) "_" }

ProgVarWildSeq :: { [ProgVar] }
  :                            { [] }
  | ProgVarWild ProgVarWildSeq {% checkDupBind $1 $2 >> return ($1 : $2) }

-- TYPE VARIABLES

TypeVar :: { TypeVar }
  : LOWER_ID { mkVar (position $1) (getText $1) }

TypeName :: { TypeVar }
  : UPPER_ID { mkVar (position $1) (getText $1) }

TypeVarBind :: { TypeVarBind }
  : TypeVar ':' Kind { TypeVarBind (position $1) $1 $3 }
  | TypeVar          { TypeVarBind (position $1) $1 (kindTU (position $1)) }

TypeNameKind :: { (TypeVar, Kind) }    -- for type and data declarations
  : TypeName ':' Kind { ($1, $3) }
  | TypeName          { ($1, kindTU (position $1)) }

TypeVarBindList :: { [TypeVarBind] }
  : TypeVarBind                     { [$1] }
  | TypeVarBind ',' TypeVarBindList {% checkDupTypeVarBind $1 $3 >> return ($1 : $3) }

TypeVarBindEmptyList :: { [TypeVarBind] }
  :                 { [] }
  | TypeVarBindList { $1 }

{
  
-----------------------
-- Parsing functions --
-----------------------
parseKind :: String -> Kind
parseKind  s = fst $ runState (parse s) (initialState "")
  where parse = kinds . scanTokens

parseType :: String -> Type
parseType s = fst $ runState (parse s) (initialState "")
  where parse = types . scanTokens

instance Read Type where
  readsPrec _ s = [(parseType s, "")]

parseTypeScheme :: String -> TypeScheme
parseTypeScheme s = fst $ runState (parse s) (initialState "")
  where parse = typeScheme . scanTokens

instance Read TypeScheme where
  readsPrec _ s = [(parseTypeScheme s, "")] 

instance Read Kind where
  readsPrec _ s = -- [(parseKind s, "")]
    tryParse [("SL", Kind defaultPos Session Lin),
              ("SU", Kind defaultPos Session Un),
              ("TL", Kind defaultPos Functional Lin),
              ("TU", Kind defaultPos Functional Un)]
    where tryParse [] = []
          tryParse ((attempt,result):xs) =
            if (take (length attempt) (trim s)) == attempt
            then [(result, drop (length attempt) (trim s))]
            else tryParse xs
          trim s = dropWhile isSpace s

parseExpr :: String -> Expression
parseExpr s = fst $ runState (parse s) (initialState "")
  where parse = expr . scanTokens
  
instance Read Expression where
  readsPrec _ s = [(parseExpr s, "")]

parseProgram :: FilePath -> Map.Map ProgVar TypeScheme -> IO FreestS
parseProgram inputFile vEnv = do
  src <- readFile inputFile
  let p = parseDefs inputFile vEnv src
  checkErrors p
  return p

parseDefs :: FilePath -> VarEnv -> String -> FreestS
parseDefs file vEnv str =
  let s = initialState file in
  execState (parse str) (s {varEnv = vEnv})
   where parse = terms . scanTokens

checkErrors :: FreestS -> IO ()
checkErrors (FreestS {errors=[]}) = return ()
checkErrors s                     = die $ intercalate "\n" (errors s)
-- checkErrors (FreestS {errors = errors})
--   | Set.null errors = return ()
--   | otherwise  = die $ show errors

-------------------
-- Handle errors --
-------------------

parseError :: [Token] -> FreestState a
parseError [] = do
  file <- getFileName
  error $ styleError file defaultPos
          ["Parse error:", styleRed "Premature end of file"]
parseError xs = do  
  f <- getFileName
  error $ styleError f p [styleRed "error\n\t", "parse error on input", styleRed $ "'" ++ show (head xs) ++ "'"]
 where p = position (head xs)

}
