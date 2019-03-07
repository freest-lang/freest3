{
module Parse.Parser (parseType, parseTypeScheme, parseDefs, parseProgram, getEPos) where
  
import           Parse.Lexer
import           Validation.TypingState (KindEnv)
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Exps
import qualified Data.Map.Strict as Map
import           Control.Monad.State
import           Data.List (nubBy, deleteFirstsBy, intercalate, find)
import           Utils.Errors
import           System.Exit (die)
import           Data.Char

}

%name types Type
%name typeScheme TypeScheme
%name terms Prog
%name kinds Kind
%name expr Expr
%tokentype { Token }
%error { parseError }
%monad { ParserState }

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
-- MAIN RULE --
---------------

Prog
  : Defs         {}
  | Defs NL Prog {}

Defs
  : DataDecl  {}
  | FunSig    {}
  | FunDecl   {}
  | TypeAbbrv {}


NL : nl NL     {}
   | nl        {}

------------------------
-- TYPE ABBREVIATIONS --
------------------------

 -- TODO: review verifications & envs added & Kind
TypeAbbrv :: { () } :
  type CONS '=' Type
    {% do
         let (TokenCons p c) = $2
         venv <- getVenv
         checkNamesClash venv c
           ("Multiple declarations of " ++ styleRed c) (posAlexToPos p)
         addToKenv c (posAlexToPos p, Kind Functional Un)
         addToVenv c (posAlexToPos p, TypeScheme [] $4)
    }

---------------
-- DATATYPES --
---------------

DataDecl :: { () } :
  data CONS '=' DataCons
    {% do
        let (TokenCons p c) = $2
        kenv <- getKenv
        checkNamesClash kenv c
          ("Multiple declarations of '" ++ styleRed c ++ "'") (posAlexToPos p)
        addToKenv c (posAlexToPos p, Kind Functional Un)
        let binds = typesToFun (posAlexToPos p) c $4
        checkBindsClash binds
        mapM (\(cons, (p, t)) -> addToCenv cons p (TypeScheme [] t)) binds
        addToVenv c (posAlexToPos p, TypeScheme [] (convertDT (posAlexToPos p) binds))
    }

DataCons :: { [(Constructor, (Pos, [Type]))] }
  : DataCon {[$1]}
  | DataCons '|' DataCon {$1 ++ [$3]}

DataCon :: { (Constructor, (Pos, [Type])) } :
  CONS TypeParams  {let (TokenCons p x) = $1 in (x,(posAlexToPos p,$2))}


TypeParams :: { [Type] }
  : {- empty -} { [] }
  | TypeParams Type  { $1 ++ [$2] }

---------------------------
-- FUN TYPE DECLARATIONS --
---------------------------

FunSig :: { () } :
  VAR ':' FunTypeScheme
    {% do
        let (TokenVar p f) = $1
        venv <- getVenv
        checkNamesClash venv f
          ("Duplicate type signatures for " ++ styleRed f) (posAlexToPos p)
        addToVenv f (posAlexToPos p, $3) 
    }

FunTypeScheme :: { TypeScheme }
  : TypeScheme { $1 }
  | Type       { TypeScheme [] $1 }

----------------------
-- FUN DECLARATIONS --
----------------------

FunDecl :: { () } :
  VAR Params '=' Expr
    {% do
        let (TokenVar p x) = $1
        addToEenv x (posAlexToPos p, $2, $4)
    }

Params :: { Params }
  : {- empty -}   {[]}
  | Params VAR    {% do
                      let (TokenVar p x) = $2
                      checkParamClash $1 (Param (posAlexToPos p) x)                      
                  }


-----------------
-- EXPRESSIONS --
-----------------


Expr :: { Expression }
  : let VAR '=' Expr in Expr         {let (TokenLet p) = $1 in
                                      let (TokenVar px x) = $2 in
				      UnLet (posAlexToPos p) (posAlexToPos px,x) $4 $6}    

  | let VAR ',' VAR '=' Expr in Expr {let (TokenLet p) = $1 in
          		                 let (TokenVar px x) = $2 in
					 let (TokenVar py y) = $4 in
		          		 BinLet (posAlexToPos p) (posAlexToPos px,x) (posAlexToPos py,y) $6 $8}

  | '(' Expr ',' Expr ')'            {let (TokenLParen p) = $1 in
                                         Pair (posAlexToPos p) $2 $4}

  | if Expr then Expr else Expr      {let (TokenIf p) = $1 in
                                         Conditional (posAlexToPos p) $2 $4 $6}

  | new Type                        {New (getPos $1) $2}
     
  | receive Expr                     {Receive (getPos $1) $2}

  | select CONS Expr                 {let (TokenCons _ x) = $2 in
					   Select (getPos $1) x $3}

  | match Expr with MatchMap         {Match (getPos $1) $2 $4}

  | fork Expr                        {Fork (getPos $1) $2}

  | case Expr of CaseMap             {Case (getPos $1) $2 $4}

  | Form {$1}


Form :: { Expression }
  : '-' Form %prec NEG {App (getPos $1) (Variable (getPos $1) "negate") $2}
  | Form '+' Form      {App (getEPos $1) (App (getEPos $1) (Variable (getPos $2) "(+)") $1) $3}
  | Form '-' Form      {App (getEPos $1) (App (getEPos $1) (Variable (getPos $2) "(-)") $1) $3}
  | Form '*' Form      {App (getEPos $1) (App (getEPos $1) (Variable (getPos $2) "(*)") $1) $3}
  | Form OP Form       {let (TokenOp p s) = $2 in
                          App (getEPos $1) (App (getEPos $1) (Variable (posAlexToPos p) s) $1) $3}
  | Juxt               {$1}       


Juxt :: { Expression }
  : Juxt Atom            {App (getEPos $1) $1 $2}

  | VAR '[' TypeList ']'  {let (TokenVar p x) = $1 in
                            TypeApp (posAlexToPos p) (Variable (posAlexToPos p) x) $3}  

  | send Atom Atom        {Send (getPos $1) $2 $3}
   
  | Atom                  {$1}

Atom :: { Expression }
  : '()'            { Unit (getPos $1) } 
  | NUM             { let (TokenInteger p x) = $1 in Integer (posAlexToPos p) x }
  | BOOL            { let (TokenBool p x) = $1 in Boolean (posAlexToPos p) x }
  | CHAR            { let (TokenChar p x) = $1 in Character (posAlexToPos p) x }
  | VAR             { let (TokenVar p x) = $1 in Variable (posAlexToPos p) x }
  | CONS            { let (TokenCons p x) = $1 in Constructor (posAlexToPos p) x }
  | '(' Expr ')'    { $2 }


TypeList :: { [Type] }
  : Type                 {[$1]}
  | TypeList ',' Type    {$1 ++ [$3]}


MatchMap :: { MatchMap } :
  MatchValue MatchNext  { Map.union $1 $2 }

MatchNext :: { MatchMap }
  : ';' MatchMap  { $2 }
  | {- empty -}   { Map.empty }

MatchValue :: { MatchMap } :
  CONS VAR '->' Expr   {let (TokenCons _ c) = $1 in
                        let (TokenVar p x) = $2 in
                        Map.singleton c (Param (posAlexToPos p) x,$4)}

CaseMap :: { CaseMap }
  : CaseValue CaseNext  { Map.union $1 $2 }

CaseNext  :: { CaseMap }
  : ';' CaseMap         { $2 }
  | {- empty -}         { Map.empty }

CaseValue :: { CaseMap } : 
  CONS Params '->' Expr   {let (TokenCons _ c) = $1 in Map.singleton c ($2,$4)}

-----------
-- TYPES --
-----------

TypeScheme :: { TypeScheme } :
  forall BindList '=>' Type {TypeScheme $2 $4}

BindList :: { [Bind] }
  : Bind               { [$1] }
  | BindList ',' Bind  {% checkBindClash $1 $3 }

Bind :: { Bind }
  : VAR KindUn    {let (TokenVar p x) = $1 in Bind x (posAlexToPos p) $2}


Type :: { Type }
  : rec VarCons KindUn '.' Type  { Rec (getPos $1) (Bind (snd $2) (fst $2) $3) $5 } -- TODO: rec VAR apenas?
  | Type ';' Type                { Semi (getPos $2) $1 $3 }
  | Type Multiplicity Type       { Fun (fst $2) (snd $2) $1 $3 }
  | '(' Type ',' Type ')'        { PairType (getPos $1) $2 $4 }
  | Polarity BasicType           { Message (fst $1) (snd $1) (snd $2) }
  | '[' FieldList ']'            { Datatype (getPos $1) $2 }
  | ChoiceView '{' FieldList '}' { Choice (fst $1) (snd $1) $3 } --{ checkClash (fst $1) (snd $1) $3 }
  | dualof Type                  { Dualof (getPos $1) $2 }
  | Skip                         { Skip (getPos $1) }
  | BasicType                    { uncurry Basic $1 }
  | VAR                          { let (TokenVar p x) = $1 in Var (posAlexToPos p) x }
  | CONS                         { let (TokenCons p x) = $1 in Var (posAlexToPos p) x }
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
  
-- TODO: add position
-- Either a var or a constructor
VarCons :: { (Pos,String) }
  : VAR  {let (TokenVar p x) = $1 in (posAlexToPos p, x) }
  | CONS {let (TokenCons p x) = $1 in (posAlexToPos p, x) }

FieldList :: { TypeMap }-- { [(Constructor, Type)] }
  : Field                { uncurry Map.singleton (snd $1) }
  | FieldList ',' Field  {% checkLabelClash $1 $3 }

Field :: { (Pos, (Constructor, Type)) }
  : CONS ':' Type { let (TokenCons p x) = $1 in (posAlexToPos p, (x, $3)) }

BasicType :: { (Pos, BasicType) }
  : Int  { (getPos $1, IntType) }
  | Char { (getPos $1, CharType) }
  | Bool { (getPos $1, BoolType) }
  | '()' { (getPos $1, UnitType) }

KindUn :: { Kind } :
    ':' SU   { Kind Session Un }
  | ':' SL   { Kind Session Lin }
  | ':' TU   { Kind Functional Un }
  | ':' TL   { Kind Functional Lin }
  |          { Kind Session Un }

Kind :: { Kind } :
    SU   {Kind Session Un}
  | SL   {Kind Session Lin}
  | TU   {Kind Functional Un}
  | TL   {Kind Functional Lin}


{

checkParamClash :: Params -> Param -> ParserState Params
checkParamClash ps p =
  case find (== p) ps of
    Just x -> do
      file <- getFileName
      addError $ styleError file (pos x)
                 ["Conflicting definitions for argument", styleRed $ "'" ++ show p ++ "'\n\t",
                  "Bound at:", file ++ ":" ++ prettyPos (pos x) ++ "\n\t",
                  "          " ++ file ++ ":" ++ prettyPos (pos p)]
      return $ ps
    Nothing -> return $ ps ++ [p]

checkLabelClash :: TypeMap -> (Pos, (Constructor, Type)) -> ParserState TypeMap
checkLabelClash m1 (p, (c,t)) = -- map position?
  case m1 Map.!? c of
    Just x -> do
      file <- getFileName
      addError $ styleError file (typePos x)
               ["Conflicting definitions for label", styleRed $ "'" ++ c ++ "'\n\t",
                "Bound at:", file ++ ":" ++ prettyPos (typePos x) ++ "\n\t",
                "          " ++ file ++ ":" ++ prettyPos p]
      return m1
    Nothing ->
      return $ Map.insert c t m1  


checkBindClash :: [Bind] -> Bind -> ParserState [Bind]
checkBindClash bs b@Bind{var=x, p=pb} =
  case find (\b1 -> var b1 == var b) bs of
    Just b1 -> do
      file <- getFileName
      addError $ styleError file (p b1)
               ["Conflicting definitions for bind", styleRed $ "'" ++ x ++ "'\n\t",
                "Bound at:", file ++ ":" ++ prettyPos (p b1) ++ "\n\t",
                "          " ++ file ++ ":" ++ prettyPos pb]
      return bs      
    Nothing -> return $ bs ++ [b]  
  
------------------------
-- Handle Parse Monad --
------------------------
type Errors = [String]
type ParserOut = (String, VarEnv, ExpEnv, ConstructorEnv, KindEnv, Errors)
type ParserState = State ParserOut

initialState :: String -> VarEnv -> ParserOut
initialState s venv = (s,venv,Map.empty,Map.empty,Map.empty, [])

addToKenv :: TypeVar -> (Pos, Kind) -> ParserState ()
addToKenv x k = modify (\(f, venv, eenv, cenv, kenv, err) ->
                          (f, venv, eenv, cenv, Map.insert x k kenv, err))

getKenv :: ParserState KindEnv
getKenv = do
  (_,_,_,_,kenv,_) <- get
  return kenv


addToVenv :: TermVar -> (Pos, TypeScheme) -> ParserState ()
addToVenv x p =
  modify (\(f, venv, eenv, cenv, kenv, err) ->
            (f, Map.insert x p venv, eenv, cenv, kenv, err))

getVenv :: ParserState VarEnv
getVenv = do
  (_,venv,_,_,_,_) <- get
  return venv
  
addToCenv :: TermVar -> Pos -> TypeScheme -> ParserState ()
addToCenv x p t = modify (\(f, venv, eenv, cenv, kenv, err) ->
                            (f, venv, eenv, Map.insert x (p, t) cenv, kenv, err))

getCenv :: ParserState ConstructorEnv
getCenv = do
  (_,_,_,cenv,_,_) <- get
  return cenv


addToEenv :: TermVar -> (Pos, Params, Expression) -> ParserState ()
addToEenv x t = modify (\(f, venv, eenv, cenv, kenv, err) ->
                          (f, venv, Map.insert x t eenv, cenv, kenv, err))

getFileName :: ParserState String
getFileName = do
  (f,_,_,_,_,_) <- get
  return f

addError :: String -> ParserState ()
addError e = modify (\(f, venv, eenv, cenv, kenv, err) ->
                          (f, venv, eenv, cenv, kenv, e : err))
-----------------------
-- Parsing functions --
-----------------------
parseKind :: String -> Kind
parseKind  s = fst $ runState (parse s) (initialState "" Map.empty)
  where parse = kinds . scanTokens

parseType :: String -> Type
parseType s = fst $ runState (parse s) (initialState "" Map.empty)
  where parse = types . scanTokens

instance Read Type where
  readsPrec _ s = [(parseType s, "")]

parseTypeScheme :: String -> TypeScheme
parseTypeScheme s = fst $ runState (parse s) (initialState "" Map.empty)
  where parse = typeScheme . scanTokens

instance Read TypeScheme where
  readsPrec _ s = [(parseTypeScheme s, "")]

-- TODO: move to kinds ??
instance Read Kind where
  readsPrec _ s = -- [(parseKind s, "")]    
    tryParse [("SL", Kind Session Lin),
              ("SU", Kind Session Un),
              ("TL", Kind Functional Lin),
              ("TU", Kind Functional Un)]
    where tryParse [] = []
          tryParse ((attempt,result):xs) =
            if (take (length attempt) (trim s)) == attempt
            then [(result, drop (length attempt) (trim s))]
            else tryParse xs
          trim s = dropWhile isSpace s


parseExpr :: String -> Expression
parseExpr s = fst $ runState (parse s) (initialState "" Map.empty)
  where parse = expr . scanTokens
  
instance Read Expression where
  readsPrec _ s = [(parseExpr s, "")]
  
-- parseDefs file str = p str 
--  where p = scanTokens
parseDefs file venv str = execState (parse str) (initialState file venv)
   where parse = terms . scanTokens
               
parseProgram inputFile venv = do
  src <- readFile inputFile
  let p = parseDefs inputFile venv src
  checkErrors p
  return p
  -- let p = parseDefs inputFile src
  -- error $ show p
  -- return (initialState "FILE")


checkErrors (_,_,_,_,_,[]) = return ()
checkErrors (_,_,_,_,_,err) = die $ intercalate "\n" err

-------------------
-- Handle errors --
-------------------

-- TODO: Pos (0,0)
parseError :: [Token] -> ParserState a
parseError [] = do
  file <- getFileName
  error $ styleError file (0,0)
          ["Parse error:", styleRed "Premature end of file"]
parseError xs = do  
  f <- getFileName
  error $ styleError f p
          [styleRed "error\n\t", "parse error on input", styleRed $ "'" ++ show (head xs) ++ "'"]
 where p = getPos (head xs)


checkNamesClash :: Map.Map TermVar (Pos, a) -> TermVar -> String -> Pos -> ParserState ()
checkNamesClash m t msg p = do
  file <- getFileName
  if Map.member t m then
    let (p1,_) = m Map.! t in
      addError $ styleError file p
                 [msg, "\n\t at " ++ file ++ prettyPos p1 ++ "\n\t    " ++ file ++ prettyPos p]
      
  else
    return ()


checkBindsClash :: [(TermVar, (Pos, Type))] -> ParserState ()
checkBindsClash binds =
  let bs = bindClashes binds in
  if not $ null bs then
    do 
      -- TODO change this
      -- fake map in reverse to get positions from the
      -- first elements (fromList keeps the last one)
      -- ... bs contains the last ones
      let tmp = Map.fromList (reverse binds)
      mapM (\(v,(p,_)) -> checkNamesClash tmp v (err v) p) bs

      return ()
  else
    do
      cenv <- getCenv
      mapM (\(v,(p,_)) -> checkNamesClash cenv v (err v) p) binds
      return ()
  where
    bindClashes :: [(TermVar, (Pos, Type))] -> [(TermVar, (Pos, Type))]
    bindClashes bs = deleteFirstsBy f bs (nubBy f bs)

    f = (\(x,_) (y,_) -> x == y)

    err v = "Multiple declarations of " ++ (styleRed ("'" ++ v ++ "'"))

   -- bindClashes :: [TermVar] -> [TermVar]
   --  bindClashes bs = bs \\ nub bs

-------------------------
-- Auxiliary functions -- TODO: all functions are auxiliar
-------------------------
  
typesToFun :: Pos -> TypeVar -> [(TypeVar, (Pos, [Type]))] -> [(TypeVar, (Pos, Type))]
typesToFun p tv = foldl (\acc (k,(p,ts)) -> acc ++ [(k, (p, typeToFun p tv ts))]) [] 
  where
    typeToFun :: Pos -> TypeVar -> [Type] -> Type
    typeToFun p c [] = (Var p c)
    typeToFun p c (x:xs) = Fun (typePos x) Un x (typeToFun p c xs)

convertDT :: Pos -> [(TypeVar,(Pos, Type))] -> Type
convertDT p ts = Datatype p $ Map.fromList $ removePos
  where
    removePos = map (\(x,(_,t)) -> (x,t)) ts

}
