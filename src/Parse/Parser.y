{
module Parse.Parser
(parseType,
 parseTypeScheme,
 parseDefs,
 parseProgram) where
  
import           Syntax.Programs
import           Syntax.Exps
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Position
import           Validation.Kinding
import           Utils.Errors
import           Utils.FreestState
import           Parse.Lexer
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
  nl       {TokenNL _}
  Int      {TokenIntT _}
  Char     {TokenCharT _}
  Bool     {TokenBoolT _}
  '()'     {TokenUnitT _}
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
  NUM      {TokenInteger _ _ }
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

%right in
%nonassoc nl new send OP -- '<' '>'
%right '->' '-o' in
%left ';'
%left '+' '-'
%left '*' '/' '%'

%nonassoc '.'
%left NEG

%%

---------------
-- PROGRAM --
---------------

Prog
  : Decl      {}
  | Decl NL Prog {}

NL : nl NL     {} -- TODO: Remove
   | nl        {}

Decl
  : DataDecl  {}
  | FunSig    {}
  -- Function Declaration
  | VarBind VarBindSeq '=' Expr {% addToEenv $1 ($2, $4) }
  | TypeAbbrv {}


------------------------
-- TYPE ABBREVIATIONS --
------------------------

 -- TODO: review verifications & envs added & Kind
TypeAbbrv :: { () } : -- TODO: the position is taken from $1
  type ConsBind '=' Type
    {% do
         venv <- getVenv
         checkNamesClash venv $2 ("Multiple declarations of " ++ styleRed (show $2))
	 addToKenv $2 (Kind (position $2) Functional Un)
	 addToVenv $2 (TypeScheme (position $4) [] $4) 
    }

---------------
-- DATATYPES --
---------------

DataDecl :: { () } : -- TODO: check positions
  data ConsBind '=' DataCons
    {% do
         let bs = typesToFun $2 $4
         venv <- getVenv
         checkNamesClash venv $2 ("Multiple declarations of " ++ styleRed (show $2))
         addToVenv $2 (TypeScheme (position $2) [] (Datatype (position $2) (Map.fromList bs)))
         checkClashes $2 bs
         addToKenv $2 (Kind (getPos $1) Functional Un)
       	 addListToCenv bs
       	 addListToVenv bs
    }

DataCons :: { [(Bind, [Type])] }
  : DataCon              { [$1] }
  | DataCon '|' DataCons { $1 : $3 }

DataCon :: { (Bind, [Type]) } :
  ConsBind TypeSeq  { ($1, $2) }

---------------------------
-- FUN SIGNATURES --
---------------------------

FunSig :: { () } :
  VarBind ':' TypeScheme
   {% do
       venv <- getVenv
       checkNamesClash venv $1 ("Duplicate type signatures for " ++ styleRed (show $1))
       addToVenv $1 $3
   }

-----------------
-- EXPRESSIONS --
-----------------

Expr :: { Expression }
  : let VarBind '=' Expr in Expr             { UnLet (getPos $1) $2 $4 $6 }    
  | let VarBind ',' VarBind '=' Expr in Expr { BinLet (getPos $1) $2 $4 $6 $8 }
  | '(' Expr ',' Expr ')'                    { Pair (getPos $1) $2 $4 }
  | if Expr then Expr else Expr              { Conditional (getPos $1) $2 $4 $6 }
  | new Type                                 { New (getPos $1) $2 }
  | receive Expr                             { Receive (getPos $1) $2 }
  | select CONS Expr                         { let (TokenCons _ x) = $2 in Select (getPos $1) x $3 }
  | match Expr with MatchMap                 { Match (getPos $1) $2 $4 }
  | fork Expr                                { Fork (getPos $1) $2 }
  | case Expr of CaseMap                     { Case (getPos $1) $2 $4 }
  | Form                                     { $1 }

Form :: { Expression }
  : '-' Form %prec NEG {App (getPos $1) (Variable (getPos $1) "negate") $2}
  | Form '+' Form      {App (position $1) (App (position $1) (Variable (getPos $2) "(+)") $1) $3}
  | Form '-' Form      {App (position $1) (App (position $1) (Variable (getPos $2) "(-)") $1) $3}
  | Form '*' Form      {App (position $1) (App (position $1) (Variable (getPos $2) "(*)") $1) $3}
  | Form OP Form       {let (TokenOp p s) = $2 in
                          App (position $1) (App (position $1) (Variable p s) $1) $3}
  | Juxt               {$1}       

Juxt :: { Expression }
  : Juxt Atom            { App (position $1) $1 $2 }
  | VAR '[' TypeList ']' { let (TokenVar p x) = $1 in TypeApp p x $3 }  
  | send Atom Atom       { Send (getPos $1) $2 $3 }
  | Atom                 { $1 }

Atom :: { Expression }
  : '()'            { Unit (getPos $1) } 
  | NUM             { let (TokenInteger p x) = $1 in Integer p x }
  | BOOL            { let (TokenBool p x) = $1 in Boolean p x }
  | CHAR            { let (TokenChar p x) = $1 in Character p x }
  | VAR             { let (TokenVar p x) = $1 in Variable p x }
  | CONS            { let (TokenCons p x) = $1 in Constructor p x }
  | '(' Expr ')'    { $2 }

MatchMap :: { MatchMap }
  : MatchValue              { $1 }
  | MatchValue ';' MatchMap { Map.union $1 $3 } -- TODO: check duplicates

MatchValue :: { MatchMap } :
  CONS VarBind '->' Expr { let (TokenCons _ c) = $1 in Map.singleton c ($2, $4) }

CaseMap :: { CaseMap }
  : Case  { $1 }
  | Case ';' CaseMap  { Map.union $1 $3 } -- TODO: check duplicates

Case :: { CaseMap }
  : CONS VarBindSeq '->' Expr { let (TokenCons _ c) = $1 in Map.singleton c ($2, $4) }

-- VARIABLES AND CONSTRUCTORS IN BINDING POSITIONS

VarBind :: { Bind }
  : VAR { let (TokenVar p x) = $1 in Bind p x }
  | '_' { Bind (getPos $1) "_" } -- TODO: rename to unique Var

ConsBind :: { Bind }
  : CONS { let (TokenCons p x) = $1 in Bind p x }

VarKBind :: { KBind }
  : VAR ':' Kind { let (TokenVar p x) = $1 in KBind p x $3 }
  | VAR		 { let (TokenVar p x) = $1 in KBind p x (Kind p Session Lin) } -- TODO: change to Functional Lin

VarBindSeq :: { [Bind] }
  :                    { [] }
  | VarBind VarBindSeq {% checkParamClash $2 $1 } -- TODO: check duplicates

VarKBindList :: { [KBind] }
  : VarKBind                  { [$1] }
  | VarKBind ',' VarKBindList {% checkKBindClash $1 $3 }

-----------
-- TYPE SCHEMES --
-----------

TypeScheme :: { TypeScheme }
  : forall VarKBindList '=>' Type { TypeScheme (getPos $1) $2 $4 }
  | Type                          { TypeScheme (position $1) [] $1 }

-----------
-- TYPES --
-----------

Type :: { Type }
  : rec VAR '.' Type             { let (TokenVar _ x) = $2 in Rec (getPos $1) x $4 } 
  | Type ';' Type                { Semi (getPos $2) $1 $3 }
  | Type Multiplicity Type       { uncurry Fun $2 $1 $3 }
  | '(' Type ',' Type ')'        { PairType (getPos $3) $2 $4 }
  | Polarity BasicType           { uncurry Message $1 (snd $2) }
  | '[' FieldList ']'            { Datatype (getPos $1) $2 }
  | ChoiceView '{' FieldList '}' { uncurry Choice $1 $3 } 
  | dualof Type                  { Dualof (getPos $1) $2 }
  | Skip                         { Skip (getPos $1) }
  | BasicType                    { uncurry Basic $1 }
  | VAR                          { let (TokenVar p x) = $1 in Var p x }
  | CONS                         { let (TokenCons p x) = $1 in Var p x }
  | '(' Type ')'                 { $2 }

Polarity :: { (Pos, Polarity) }
  : '?' { (getPos $1, In) }
  | '!' { (getPos $1, Out) }

Multiplicity :: { (Pos, Multiplicity) }
  : '->' { (getPos $1, Un) }
  | '-o' { (getPos $1, Lin) }

ChoiceView :: { (Pos, ChoiceView) }
  : '+' { (getPos $1, Internal) }
  | '&' { (getPos $1, External) }
  
FieldList :: { TypeMap }
  : Field                { $1 }
  | Field ',' FieldList  {% checkLabelClash $1 $3 }

Field :: { TypeMap }
  : CONS ':' Type { let (TokenCons p x) = $1 in Map.singleton (Bind p x) $3 }

BasicType :: { (Pos, BasicType) }
  : Int  { (getPos $1, IntType) }
  | Char { (getPos $1, CharType) }
  | Bool { (getPos $1, BoolType) }
  | '()' { (getPos $1, UnitType) }

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
    SU {Kind (getPos $1) Session Un}
  | SL {Kind (getPos $1) Session Lin}
  | TU {Kind (getPos $1) Functional Un}
  | TL {Kind (getPos $1) Functional Lin}

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
    tryParse [("SL", Kind (AlexPn (-1) (-1) (-1)) Session Lin),
              ("SU", Kind (AlexPn (-1) (-1) (-1)) Session Un),
              ("TL", Kind (AlexPn (-1) (-1) (-1)) Functional Lin),
              ("TU", Kind (AlexPn (-1) (-1) (-1)) Functional Un)]
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

-- TODO: Pos (0,0)
parseError :: [Token] -> FreestState a
parseError [] = do
  file <- getFileName
  error $ styleError file (AlexPn 0 0 0)
          ["Parse error:", styleRed "Premature end of file"]
parseError xs = do  
  f <- getFileName
  error $ styleError f p [styleRed "error\n\t", "parse error on input", styleRed $ "'" ++ show (head xs) ++ "'"]
 where p = getPos (head xs)


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
--     removePos = map (\(x,(_,t)) -> (Bind (AlexPn 0 0 0) x,t)) ts -- TODO: tmp pos

}
