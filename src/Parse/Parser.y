{
module Parse.Parser
( parseType
, parseTypeScheme
, parseDefs
, parseProgram
) where
  
import           Parse.Lexer
import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Bind
import           Validation.Kinding
import           Utils.Errors
import           Utils.FreestState
import           Parse.ParserUtils
import           Control.Monad.State
import           Data.Char
import           Data.List (nub, (\\), intercalate, find)
import qualified Data.Map.Strict as Map
import           System.Exit (die)

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
  CONS     {TokenCons _ _}
  VAR      {TokenVar _ _}
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
%left fork send receive select
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
  : DataDecl  {}
  | VarBind ':' TypeScheme -- Function signature
    {% checkDupTypeSig $1 >> addToVenv $1 $3 }
  | VarBind VarBindSeq '=' Expr -- Function declaration
    {% checkDupFunDecl $1 >> addToEenv $1 ($2, $4) }
  | type ConsBind VarKindEmptyList '=' Type -- Type abbreviation
    {% checkDupTypeDecl $2 >> addToVenv $2 (TypeScheme (position $1) $3 $5) }

---------------
-- DATATYPES --
---------------

-- TODO: check positions
-- TODO: not need to add to kind env
DataDecl :: { () }
  : data ConsBind VarKindEmptyList '=' DataCons
    {% do
       let bs = typesToFun $2 $5
       checkDupTypeDecl $2
       addToVenv $2 (TypeScheme (position $2) $3 (Datatype (position $2) (Map.fromList bs)))
       checkClashes $2 bs
       addToKenv $2 (Kind (position $1) Functional Un)
       addListToCenv bs
       addListToVenv bs
    }

DataCons :: { [(Bind, [Type])] }
  : DataCon              { [$1] }
  | DataCon '|' DataCons { $1 : $3 }

DataCon :: { (Bind, [Type]) }
  : ConsBind TypeSeq  { ($1, $2) }

-----------------
-- EXPRESSIONS --
-----------------

Expr :: { Expression }
  : let VarBind '=' Expr in Expr             { UnLet (position $1) $2 $4 $6 }
  | let VarBind ',' VarBind '=' Expr in Expr { BinLet (position $1) $2 $4 $6 $8 }
  | if Expr then Expr else Expr              { Conditional (position $1) $2 $4 $6 }
  | new Type                                 { New (position $1) $2 }
  | match Expr with '{' MatchMap '}'         { Match (position $1) $2 $5 }
  | case Expr of '{' CaseMap '}'             { Case (position $1) $2 $5 }
  | Expr '*' Expr                            { binOp $1 (position $2) "(*)" $3 }
  | Expr '+' Expr                            { binOp $1 (position $2) "(+)" $3 }
  | Expr '-' Expr                            { binOp $1 (position $2) "(-)" $3 }
  | Expr OP Expr                             { binOp $1 (position $2) (getText $2) $3 }
  | App                                      { $1 }

App :: { Expression }
  : App Primary                              { App (position $1) $1 $2 }
  | VAR '[' TypeList ']'                     { TypeApp (position $1) (getText $1) $3 }
  | send Primary                             { Send (position $1) $2 }
  | receive Primary                          { Receive (position $1) $2 }
  | select CONS Primary                      { Select (position $1) (getText $2) $3 }
  | fork Primary                             { Fork (position $1) $2 }
  | '-' App %prec NEG                        { unOp (position $1) "negate" $2}
  | Primary                                  { $1 }

Primary :: { Expression }
  : INT                                      { let (TokenInteger p x) = $1 in Integer p x }
  | BOOL                                     { let (TokenBool p x) = $1 in Boolean p x }
  | CHAR                                     { let (TokenChar p x) = $1 in Character p x }
  | '()'                                     { Unit (position $1) }
  | VAR                                      { Variable (position $1) (getText $1) }
  | CONS                                     { Constructor (position $1) (getText $1) }
  | '(' Expr ',' Expr ')'                    { Pair (position $1) $2 $4 }
  | '(' Expr ')'                             { $2 }

MatchMap :: { MatchMap }
  : Match              { uncurry Map.singleton $1 }
  | Match ';' MatchMap {% checkDupMatch (fst $1) $3 >>
                          return (uncurry Map.insert $1 $3) }

Match :: { (Bind, (Bind, Expression)) }
  : ConsBind VarBind '->' Expr { ($1, ($2, $4)) }

CaseMap :: { CaseMap }
  : Case             { uncurry Map.singleton $1 }
  | Case ';' CaseMap {% checkDupMatch (fst $1) $3 >>
                        return (uncurry Map.insert $1 $3) }
                        
Case :: { (Bind, ([Bind], Expression)) }
  : ConsBind VarBindSeq '->' Expr { ($1, ($2, $4)) }

-----------
-- TYPE SCHEMES --
-----------

TypeScheme :: { TypeScheme }
  : forall VarKindList '=>' Type { TypeScheme (position $1) $2 $4 }
  | Type                          { TypeScheme (position $1) [] $1 }

-----------
-- TYPES --
-----------

Type :: { Type }
  : rec RecVar '.' Type          { Rec (position $1)  $2 $4 } 
  | Type ';' Type                { Semi (position $2) $1 $3 }
  | Type Multiplicity Type       { uncurry Fun $2 $1 $3 }
  | '(' Type ',' Type ')'        { PairType (position $3) $2 $4 }
  | Polarity BasicType           { uncurry Message $1 (snd $2) }
  | '[' FieldList ']'            { Datatype (position $1) $2 }
  | ChoiceView '{' FieldList '}' { uncurry Choice $1 $3 } 
  | dualof Type                  { Dualof (position $1) $2 }
  | Skip                         { Skip (position $1) }
  | BasicType                    { uncurry Basic $1 }
  | VAR                          { Var (position $1) (getText $1) }
  | CONS                         { Var (position $1) (getText $1) }
  | '(' Type ')'                 { $2 }

Polarity :: { (Pos, Polarity) }
  : '?' { (position $1, In) }
  | '!' { (position $1, Out) }

Multiplicity :: { (Pos, Multiplicity) }
  : '->' { (position $1, Un) }
  | '-o' { (position $1, Lin) }

ChoiceView :: { (Pos, ChoiceView) }
  : '+' { (position $1, Internal) }
  | '&' { (position $1, External) }
  
FieldList :: { TypeMap }
  : Field               { uncurry Map.singleton $1 }
  | Field ',' FieldList {% checkDupField (fst $1) $3 >>
                            return (uncurry Map.insert $1 $3) }

Field :: { (Bind, Type) }
  : ConsBind ':' Type { ($1, $3) }

BasicType :: { (Pos, BasicType) }
  : Int  { (position $1, IntType) }
  | Char { (position $1, CharType) }
  | Bool { (position $1, BoolType) }
  | '()' { (position $1, UnitType) }

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

-- VARIABLES AND CONSTRUCTORS IN BINDING POSITIONS

VarBind :: { Bind }
  : VAR { Bind (position $1) (getText $1) }
  | '_' { Bind (position $1) "_" } -- TODO: rename to unique Var

ConsBind :: { Bind }
  : CONS { Bind (position $1) (getText $1) }

VarKind :: { KBind }
  : VAR ':' Kind { KBind (position $1) (getText $1) $3 }
  | VAR		 { let p = position $1 in KBind p (getText $1) (Kind p Functional Lin) }

RecVar :: { KBind }
  : VAR ':' Kind { KBind (position $1) (getText $1) $3 }
  | VAR		 { let p = position $1 in KBind p (getText $1) (Kind p Session Lin) }

VarBindSeq :: { [Bind] }
  :                    { [] }
  | VarBind VarBindSeq {% checkDupBind $1 $2 >> return ($1 : $2) }

VarKindList :: { [KBind] }
  : VarKind                  { [$1] }
  | VarKind ',' VarKindList {% checkDupKBind $1 $3 >> return ($1 : $3) }

VarKindEmptyList :: { [KBind] }
  :              { [] }
  | VarKindList { $1 }

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

-- TODO: move to kinds ??
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

parseProgram :: FilePath -> Map.Map Bind TypeScheme -> IO FreestS
parseProgram inputFile venv = do
  src <- readFile inputFile
  let p = parseDefs inputFile venv src
  checkErrors p
  return p

parseDefs :: FilePath -> VarEnv -> String -> FreestS
parseDefs file venv str =
  let s = initialState file in
  execState (parse str) (s {varEnv=venv})
   where parse = terms . scanTokens

checkErrors (FreestS {errors=[]}) = return ()
checkErrors s = die $ intercalate "\n" (errors s)

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


-- tmp move to state
-- maybe refactor addType & addTypeScheme and then use uncurry
addListToCenv :: [(Bind,Type)] -> FreestState ()
addListToCenv bs = do
  mapM (\(b, t) -> addToCenv b (TypeScheme (position t) [] t)) bs
  return ()

addListToVenv :: [(Bind,Type)] -> FreestState ()
addListToVenv bs = do
  mapM (\(b, t) -> addToVenv b (TypeScheme (position t) [] t)) bs
  return ()

-- Converting a list of types 
  
typesToFun :: Bind -> [(Bind, [Type])] -> [(Bind, Type)]
typesToFun (Bind p x) =
  foldr (\(k,ts) acc -> (k, typeToFun p x ts) : acc) []
  where
    typeToFun :: Pos -> TypeVar -> [Type] -> Type
    typeToFun p c [] = (Var p c)
    typeToFun p c (x:xs) = Fun (position x) Un x (typeToFun p c xs)

-- convertDT :: Pos -> [(TypeVar,(Pos, Type))] -> Type
-- convertDT p ts = Datatype p $ Map.fromList $ removePos
--   where
--     removePos = map (\(x,(_,t)) -> (Bind defaultPos x,t)) ts -- TODO: tmp pos

}
