{
module Parse.Parser (parseType, parseTypeScheme, parseDefs, parseProgram, getEPos) where
  
import           Parse.Lexer
import           Validation.TypingState (KindEnv)
import           Syntax.Types
import           Syntax.Kinds
import           Syntax.Terms
import qualified Data.Map.Strict as Map
import           Control.Monad.State
import           Data.List (nubBy, deleteFirstsBy, intercalate)
import           Utils.Errors
import           System.Exit (die)
import           Data.Char
}

%name types Types
%name typeScheme TypeScheme
%name terms Prog
%name kinds Kind
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
  '+{'      {TokenLIChoice _}
  '&{'      {TokenLEChoice _}
  '}'      {TokenRBrace _}
  '=>'     {TokenFArrow _}
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
  

%right in
%nonassoc nl new send OP -- '<' '>'
%right '->' '-o' in
%left ';'
%left '+' '-'
%left '*'

%nonassoc '.'
%left NEG

%%

---------------
-- MAIN RULE --
---------------
-- TODO: Block comments with line comments inide

Prog : Defs                {% return ()}
     | Defs NL Prog        {% return ()}
    

Defs : DataDecl 
         {% do 
	     let (p,c,k,ts) = $1
             kenv <- getKenv
             checkNamesClash kenv c
               ("Multiple declarations of " ++ (styleRed ("'" ++ c ++ "'"))) p
	     addToKenv c (p, k)
	     let binds = typesToFun p c ts

             checkBindsClash binds
	     mapM (\(cons, (p, t)) -> addToCenv cons p (TypeScheme [] t)) binds
	     addToVenv c p (TypeScheme [] (convertDT p binds))
	     return ()
	 }
     | FunSig  {% do 
	           let (p,x,y) = $1
                   venv <- getVenv
                   checkNamesClash venv x
                     ("Duplicate type signatures for " ++ (styleRed ("'" ++ x ++ "'"))) p
		   addToVenv x p y
		   return ()}
     | FunDecl  {% do 
   	           let (x,y) = $1
		   addToEenv x y
		   return ()
	        }

  


NL : nl NL     {}
   | nl        {}

-- emptyNL : nl NL     {}
--         | nl        {}
--         | {-empty-} {}

---------------
-- DATATYPES --
---------------

DataDecl : data CONS KindTU '=' DataCons   {let (TokenCons p x) = $2 in (pos p,x,$3,$5)}

DataCons : DataCon {[$1]}
         | DataCons '|' DataCon {$1 ++ [$3]}

DataCon : CONS TypeParams  {let (TokenCons p x) = $1 in (x,(pos p,$2))}

TypeParams : {- empty -} { [] }
           | TypeParams Types  { $1 ++ [$2] }

KindTU :: { Kind }
KindTU
     : ':'':' SU   {Kind Session Un}
     | ':'':' SL   {Kind Session Lin}
     | ':'':' TU   {Kind Functional Un}
     | ':'':' TL   {Kind Functional Lin}
     | {- empty -} {Kind Functional Un}


---------------------------
-- FUN TYPE DECLARATIONS --
---------------------------

FunSig : VAR ':'':' FunTypeScheme  {let (TokenVar p x) = $1 in (pos p,x,$4)}

FunTypeScheme : TypeScheme {$1}
              | Types      {TypeScheme [] $1}

----------------------
-- FUN DECLARATIONS --
----------------------

FunDecl : VAR Params '=' Expr   {let (TokenVar p x) = $1 in (x,(pos p, $2, $4))}

Params : {- empty -}   {[]}
       | Params VAR    {let (TokenVar _ x) = $2 in ($1 ++ [x])}


-----------------
-- EXPRESSIONS --
-----------------


Expr : let VAR '=' Expr in Expr         {let (TokenLet p) = $1 in
				         let (TokenVar px x) = $2 in
				         UnLet (pos p) (pos px,x) $4 $6}    

     | let VAR ',' VAR '=' Expr in Expr {let (TokenLet p) = $1 in
          		                 let (TokenVar px x) = $2 in
					 let (TokenVar py y) = $4 in
		          		 BinLet (pos p) (pos px,x) (pos py,y) $6 $8}

     | '(' Expr ',' Expr ')'            {let (TokenLParen p) = $1 in
                                         Pair (pos p) $2 $4}

     | if Expr then Expr else Expr      {let (TokenIf p) = $1 in
                                         Conditional (pos p) $2 $4 $6
			                }

     | new Types                        {New (getPos $1) $2}
     

     | receive Expr                     {Receive (getPos $1) $2}

     | select CONS Expr                 {let (TokenCons _ x) = $2 in
					   Select (getPos $1) x $3}

     | match Expr with MatchMap         {Match (getPos $1) $2 (Map.fromList $4)}

     | fork Expr                        {Fork (getPos $1) $2}

     | case Expr of CaseMap             {Case (getPos $1) $2 (Map.fromList $4)}

     | Form {$1}


Form : '-' Form %prec NEG {App (getPos $1) (Variable (getPos $1) "negate") $2}
     | Form '+' Form      {App (getEPos $1) (App (getEPos $1) (Variable (getPos $2) "(+)") $1) $3}
     | Form '-' Form      {App (getEPos $1) (App (getEPos $1) (Variable (getPos $2) "(-)") $1) $3}
     | Form '*' Form      {App (getEPos $1) (App (getEPos $1) (Variable (getPos $2) "(*)") $1) $3}
     | Form OP Form       {let (TokenOp p s) = $2 in
                            App (getEPos $1) (App (getEPos $1) (Variable (pos p) s) $1) $3}
     | Juxt               {$1}       


Juxt : Juxt Atom                   {App (getEPos $1) $1 $2}

     | VAR '[' CommaTypes ']'      {let (TokenVar p x) = $1 in
                                   TypeApp (pos p) (Variable (pos p) x) $3}  

     | send Atom Atom              {Send (getPos $1) $2 $3}
   
     | Atom                        {$1}



Atom : '(' Expr ')'                {$2}

     | '()'                        {Unit (getPos $1)}

     | NUM                         {let (TokenInteger p x) = $1 in
		         	     Integer (pos p) x}

     | BOOL                        {let (TokenBool p x) = $1 in
			              Boolean (pos p) x}

     | CHAR                        {let (TokenChar p x) = $1 in
		         	     Character (pos p) x}

     | VAR                         {let (TokenVar p x) = $1 in
                                     Variable (pos p) x} 

     | CONS                        {let (TokenCons p x) = $1 in
				      Constructor (pos p) x}

CommaTypes : Types                   {[$1]}
           | CommaTypes ',' Types    {$1 ++ [$3]}



MatchMap : MatchValue MatchNext  {$1 : $2}

MatchNext : ';' MatchMap         {$2}
          | {- empty -}          {[]}

MatchValue : CONS VAR '->' Expr   {let (TokenCons _ c) = $1 in
				       let (TokenVar _ x) = $2 in
                                       (c, (x,$4))}


CaseMap : CaseValue CaseNext  {$1 : $2}

CaseNext : ';' CaseMap         {$2}
         | {- empty -}         {[]}

CaseValue : CONS Params '->' Expr   {let (TokenCons _ c) = $1 in			       
                                       (c, ($2,$4))}

-----------
-- TYPES --
-----------

TypeScheme : forall CommaBinds '=>' Types {TypeScheme $2 $4}

CommaBinds : Bind                 {[$1]}
           | CommaBinds ',' Bind  {$1 ++ [$3]}

Bind : VAR UnKind    {let (TokenVar _ x) = $1 in Bind x $2}

UnKind :: { Kind }
     : ':'':' SU   {Kind Session Un}
     | ':'':' SL   {Kind Session Lin}
     | ':'':' TU   {Kind Functional Un}
     | ':'':' TL   {Kind Functional Lin}
     | {- empty -} {Kind Session Un}

-- {let (TokenVar _ x) = $2 in (Rec (Bind x $3) $5)}
Types : '(' Types ')'                {$2}
      | rec VarCons KindSL '.' Types {Rec (getPos $1) (Bind $2 $3) $5}
      | Types ';' Types              {Semi (getPos $2) $1 $3}
      | Types '->' Types             {Fun (getPos $2) Un $1 $3}
      | Types '-o' Types             {Fun (getPos $2) Lin $1 $3}
      | '(' Types ',' Types ')'      {PairType (getPos $3) $2 $4}
      | '?' BasicType                {let (_,t) = $2 in Message (getPos $1) In t}
      | '!' BasicType                {let (_,t) = $2 in Message (getPos $1) Out t}
      | '[' Constructor ']'          {Datatype (getPos $1) (Map.fromList $2)}
      | Choice                       {$1}
      | Skip                         {Skip (getPos $1)}
      | BasicType                    {let (p,t) = $1 in Basic p t}
      | VAR                          {let (TokenVar p x) = $1 in Var (pos p) x}
      | CONS                         {let (TokenCons p x) = $1 in Var (pos p) x}

-- TODO: add position
VarCons : VAR  {let (TokenVar _ x) = $1 in x}
        | CONS {let (TokenCons _ x) = $1 in x}

Choice : '+{' Constructor '}'  {Choice (getPos $1) Internal (Map.fromList $2)}
       | '&{' Constructor '}'  {Choice (getPos $1) External (Map.fromList $2)}

Constructor : Con                  {$1}
            | Constructor ',' Con  {$3 ++ $1}

Con : CONS ':' Types {let (TokenCons _ x) = $1 in [(x, $3)]}

BasicType  : Int  {(getPos $1, IntType)}
           | Char {(getPos $1, CharType)}
           | Bool {(getPos $1, BoolType)}
           | '()' {(getPos $1, UnitType)}


KindSL :: { Kind }
     : ':'':' SU   {Kind Session Un}
     | ':'':' SL   {Kind Session Lin}
     | ':'':' TU   {Kind Functional Un}
     | ':'':' TL   {Kind Functional Lin}
     | {- empty -} {Kind Session Lin}

Kind :: { Kind }
     : SU   {Kind Session Un}
     | SL   {Kind Session Lin}
     | TU   {Kind Functional Un}
     | TL   {Kind Functional Lin}


{

-- TODO: tmp ... remove   
-- type KindEnv = Map.Map TypeVar (Pos, Kind)

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


addToVenv :: TermVar -> Pos -> TypeScheme -> ParserState ()
addToVenv x p t =
  modify (\(f, venv, eenv, cenv, kenv, err) ->
            (f, Map.insert x (p,t) venv, eenv, cenv, kenv, err))

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

parseTypeScheme :: String -> TypeScheme
parseTypeScheme s = fst $ runState (parse s) (initialState "" Map.empty)
  where parse = typeScheme . scanTokens

instance Read Type where
  readsPrec _ s = [(parseType s, "")]

instance Read TypeScheme where
  readsPrec _ s = [(parseTypeScheme s, "")]

-- [(parseKind s, "")]
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
-- Auxiliary functions --
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
