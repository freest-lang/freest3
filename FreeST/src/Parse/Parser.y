{
module Parse.Parser
-- ( parseType
-- , parseTypeScheme
-- , parseDefs
-- , parseProgram
-- , parseSchemes
--)
where

  
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
import qualified Data.Set as Set
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
%name schemes Schemes
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
  '&&'     {TokenConjunction _} 
  '||'     {TokenDisjunction _} 
  '/'      {TokenDiv _} 
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

%nonassoc LOWER_ID UPPER_ID
%nonassoc '('
%nonassoc '()'
%nonassoc '['

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
%right ':'

-- Type
%right '.'       -- Used in rec 
%right '->' '-o' -- TODO: an Expr operator as well
%right ';'       -- TODO: an Expr operator as well
%right dualof

-- Precedence of lambda expressions
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
  : ProgVar ':' TypeScheme {% do
      toStateT $ checkDupProgVarDecl $1
      toStateT $ addToVEnv $1 $3 }
  -- Function declaration
  | ProgVar ProgVarWildSeq '=' Expr {% do
      toStateT $ checkDupFunDecl $1
      e <- toStateT $ buildFunBody $1 $2 $4
      toStateT $ addToEEnv $1 e }
  -- Type abbreviation
  | type TypeNameKind TypeVarBindEmptyList '=' Type {% do
      toStateT $ checkDupTypeDecl (fst $2)
      toStateT $ uncurry addToTEnv $2 (TypeScheme (position $5) $3 $5) }
  -- Datatype declaration
  | data TypeNameKind TypeVarBindEmptyList '=' DataCons {% do
      let a = fst $2
      toStateT $ checkDupTypeDecl a
      let bs = typeListToType a $5 :: [(ProgVar, Type)]
      toStateT $ mapM_ (\(c, t) -> addToVEnv c (fromType t)) bs
      let p = position a
      toStateT $ uncurry addToTEnv $2 (TypeScheme p $3 (Datatype p (Map.fromList bs)))
    }

DataCons :: { [(ProgVar, [Type])] }
  : DataCon              {% toStateT $ checkDupCons $1 [] >> return [$1] }
  | DataCon '|' DataCons {% toStateT $ checkDupCons $1 $3 >> return ($1 : $3) }

DataCon :: { (ProgVar, [Type]) }
  : Constructor TypeSeq { ($1, $2) }

-----------------
-- EXPRESSIONS --
-----------------

Expr :: { Expression }
  : let ProgVarWild '=' Expr in Expr { UnLet (position $1) $2 $4 $6 }
  | Expr ';' Expr                    { App (position $1)
                                          (Lambda (position $1) Un
                                            (mkVar (position $1) "_")
                                            (Basic (position $3) UnitType) $3)
                                       $1}
  | let '(' ProgVarWild ',' ProgVarWild ')' '=' Expr in Expr
                                     { BinLet (position $1) $3 $5 $8 $10 }
  | if Expr then Expr else Expr      { Conditional (position $1) $2 $4 $6 }
  | new Type                         { New (position $1) $2 (Dualof (negPos (position $2)) $2) }
  | match Expr with '{' MatchMap '}' { Match (position $1) $2 $5 }
  | case Expr of '{' CaseMap '}'     { Case (position $1) $2 $5 }
  | Expr '||' Expr                   { binOp $1 (mkVar (position $2) "(||)") $3 }
  | Expr '&&' Expr                   { binOp $1 (mkVar (position $2) "(&&)") $3 }
  | Expr OP Expr                     { binOp $1 (mkVar (position $2) (getText $2)) $3 }
--  | ListFunctions                   { $1 }
  | Expr '+' Expr                    { binOp $1 (mkVar (position $2) "(+)") $3 }
  | Expr '-' Expr                    { binOp $1 (mkVar (position $2) "(-)") $3 }
  | Expr '*' Expr                    { binOp $1 (mkVar (position $2) "(*)") $3 }
  | Expr '/' Expr                    { binOp $1 (mkVar (position $2) "div") $3 }
  | Expr ':'':' Expr                 { binOp $1 (mkVar (position $2) "(::)") $4 }
  | Expr '+''+' Expr                 { binOp $1 (mkVar (position $2) "(++)") $4 }
  | App                              { $1 }

App :: { Expression }
  : App Primary                     { App (position $1) $1 $2 }
  | ProgVar '[' TypeList ']'        { TypeApp (position $1) $1 $3 }
  | send Primary                    { Send (position $1) $2 }
  | receive Primary                 { Receive (position $1) $2 }
  | select Primary ArbitraryProgVar { Select (position $1) $2 $3 }
  | fork Primary                    { Fork (position $1) $2 }
  | '-' App %prec NEG               { unOp (mkVar (position $1) "negate") $2}
  | List                            { $1 }
  | Primary                         { $1 }

Primary :: { Expression }
  : INT                                      { let (TokenInteger p x) = $1 in Integer p x }
  | BOOL                                     { let (TokenBool p x) = $1 in Boolean p x }
  | CHAR                                     { let (TokenChar p x) = $1 in Character p x }
  | '()'                                     { Unit (position $1) }
  | ArbitraryProgVar                         { ProgVar (position $1) $1 }
  -- | ProgVar                                  { ProgVar (position $1) $1 }
  -- | Constructor                              { ProgVar (position $1) $1 }
  | '(' '\\' ProgVarWildTBind Arrow Expr ')' { Lambda (position $2) (snd $4) (fst $3) (snd $3) $5 }
  | '(' Expr ',' Expr ')'                    { Pair (position $1)$2 $4 }
  | '(' Expr ')'                             { $2 }

List :: { Expression }
  : '[' ']'              { ProgVar (position $1) (mkVar (position $1) "#Nil" ) }   -- Empty case
  | '[' IntListExpr ']'  { $2 }

IntListExpr :: { Expression }
  : Expr                  { let p = position $1 in  
                            App p
                            (App p (ProgVar p (mkVar p "#Cons")) $1)
                            (ProgVar p (mkVar p "#Nil")) }
  | Expr ',' IntListExpr  { let p = position $1 in 
                            App p 
                            (App p (ProgVar p (mkVar p "#Cons")) $1) 
                            $3 }

-- ListFunctions :: { Expression }
--  | Expr ':' ':' Expr                { binOp $1 (mkVar (position $2) "(::)") $3 }
--  | Expr '+' '+' Expr                { binOp $1 (mkVar (position $2) "(++)") $3 }

ProgVarWildTBind :: { (ProgVar, Type) }
 : ProgVarWild ':' Type  %prec ProgVarWildTBind { ($1, $3) }

MatchMap :: { FieldMap }
  : Match              { uncurry Map.singleton $1 }
  | Match ',' MatchMap {% toStateT $ checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }

Match :: { (ProgVar, ([ProgVar], Expression)) }
  : ArbitraryProgVar ProgVarWild '->' Expr { ($1, ([$2], $4)) }

CaseMap :: { FieldMap }
  : Case             { uncurry Map.singleton $1 }
  | Case ',' CaseMap {% toStateT $ checkDupCase (fst $1) $3 >> return (uncurry Map.insert $1 $3) }
                        
Case :: { (ProgVar, ([ProgVar], Expression)) }
  : Constructor ProgVarWildSeq '->' Expr                { ($1, ($2, $4)) }
  | '(' ProgVarWild ':'':' ProgVarWild ')'  '->' Expr   {% toStateT $ checkDupBind $2 [$5] >>
                                                           return (mkVar (position $1) "#Cons"
                                                                  , ($2 : [$5], $8)) }
  | '['']' '->' Expr                                    { (mkVar (position $1) "#Nil", ([], $4)) }

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
  : BasicType                        { uncurry Basic $1 }
  | Type Arrow Type                  { uncurry Fun $2 $1 $3 }
  | '(' Type ',' Type ')'            { PairType (position $1) $2 $4 }
  -- Session types
  | Skip                             { Skip (position $1) }
  | Type ';' Type                    { Semi (position $2) $1 $3 }
  | Polarity BasicType               { uncurry Message $1 (snd $2) }
  | ChoiceView '{' FieldList '}'     { uncurry Choice $1 $3 } 
  | rec TypeVarBind '.' Type         { Rec (position $1) $2 $4 }
  -- Functional or session
  | TypeVar                          { TypeVar (position $1) $1 }
  -- Type operators
  | dualof Type                      { Dualof (position $1) $2 }
  | TypeName                         { TypeName (position $1) $1 }
  | '(' Type ')'                     { $2 }
  | '[' Int ']'                      { TypeName (position $1) (mkVar (position $1) "#IntList") } 
  -- TODO(J) hide this "#IntList" from Parser.y

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
  | Field ',' FieldList {% toStateT $ checkDupField (fst $1) $3 >>
                           return (uncurry Map.insert $1 $3) }

Field :: { (ProgVar, Type) }
  : ArbitraryProgVar ':' Type { ($1, $3) }

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
    SU { kindSU (position $1) }
  | SL { kindSL (position $1) }
  | TU { kindTU (position $1) }
  | TL { kindTL (position $1) }

-- PROGRAM VARIABLES

ArbitraryProgVar :: { ProgVar }
 : ProgVar     { $1 }
 | Constructor { $1 }

ProgVar :: { ProgVar }
  : LOWER_ID { mkVar (position $1) (getText $1) }

Constructor :: { ProgVar }
  : UPPER_ID { mkVar (position $1) (getText $1) }

ProgVarWild :: { ProgVar }
  : ProgVar { $1 }
  | '_'     { mkVar (position $1) "_" }

ProgVarWildSeq :: { [ProgVar] }
  :                            { [] }
  | ProgVarWild ProgVarWildSeq {% toStateT $ checkDupBind $1 $2 >> return ($1 : $2) }

-- TYPE VARIABLES

TypeVar :: { TypeVar }
  : LOWER_ID { mkVar (position $1) (getText $1) }

TypeName :: { TypeVar }
  : UPPER_ID { mkVar (position $1) (getText $1) }

TypeVarBind :: { TypeVarBind }
  : TypeVar ':' Kind { TypeVarBind (position $1) $1 $3 }
  | TypeVar          { TypeVarBind (position $1) $1 (omission (position $1)) }

TypeNameKind :: { (TypeVar, Kind) }    -- for type and data declarations
  : TypeName ':' Kind { ($1, $3) }
  | TypeName          { ($1, omission (position $1)) }

TypeVarBindList :: { [TypeVarBind] }
  : TypeVarBind                     { [$1] }
  | TypeVarBind ',' TypeVarBindList {% toStateT $ checkDupTypeVarBind $1 $3 >> return ($1 : $3) }

TypeVarBindEmptyList :: { [TypeVarBind] }
  :                 { [] }
  | TypeVarBindList { $1 }


--------------------
-- SCHEMES PARSER --
--------------------
Schemes :: { (TypeScheme, TypeScheme) }
  : TypeScheme NL TypeScheme { ($1, $3) }


{
  
-----------------------
-- Parsing functions --
-----------------------
parseKind :: String -> Kind
parseKind str =
  case evalStateT (parse str "" kinds) (initialState "") of
    Ok x -> x
    Failed err -> error err

parseType :: String -> Either Type String
parseType str =
  case runStateT (parse str "" types) (initialState "") of
    Ok (t, state) -> eitherTypeErr t state
    Failed err -> Right $ err      
  where
    eitherTypeErr t state
      | hasErrors state = Right $ getErrors state
      | otherwise       = Left $ t

instance Read Type where
  readsPrec _ str =
    case runStateT (parse str "" types) (initialState "") of
      Ok (t, state) ->
        if hasErrors state then error $ getErrors state else [(t, "")]
      Failed err -> error err

----------------------
-- PARSING SCHEMES  --
----------------------

parseSchemes :: String -> String -> Either (TypeScheme, TypeScheme) String
parseSchemes file str =
  case runStateT (parse str file schemes) (initialState file) of
    Ok (t,s) ->
      if hasErrors s then Right (getErrors s) else Left t
    Failed err -> Right err

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

-----------------------
-- PARSING PROGRAMS  --
-----------------------

parseProgram :: FilePath -> Map.Map ProgVar TypeScheme -> IO FreestS
parseProgram inputFile vEnv = do
  src <- readFile inputFile
  return $ parseDefs inputFile vEnv src

parseDefs :: FilePath -> VarEnv -> String -> FreestS
parseDefs file vEnv str =
  let s = initialState file in
  case execStateT (parse str file terms) (s {varEnv = vEnv}) of
    Ok s1 -> s1
    Failed err -> s {errors = (errors s) ++ [err]}
   -- where
   --   parse = lexer str file terms

parse str file f = lexer str file f

lexer str file f = 
  case scanTokens str file of
    Right err -> failM err
    Left x    -> f x

-- checkErrors :: FreestS -> IO ()
-- checkErrors s
--   | hasErrors s = die $ getErrors s
--   | otherwise   = return ()

-------------------
-- Handle errors --
-------------------

parseError :: [Token] -> FreestStateT a
parseError [] = do
  file <- toStateT getFileName
  failM $ formatErrorMessages Map.empty defaultPos file
          [Error "Parse error:", Error "\ESC[91mPremature end of file\ESC[0m"]
parseError xs = do  
  file <- toStateT getFileName
  failM $ formatErrorMessages Map.empty p file
    [Error "Parse error on input", Error $ "\ESC[91m'" ++ show (head xs) ++ "'\ESC[0m"]
 where p = position (head xs)

failM :: String -> FreestStateT a
failM err = lift $ Failed err

toStateT x = state $ runState x

}
