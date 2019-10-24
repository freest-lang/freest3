import           Syntax.Types
import           Syntax.Base
import           Syntax.Kinds
import           Equivalence.Bisimulation
import qualified Data.Map.Strict as Map

{-
Remember:

Types must be

* contractive, and
* renamed, so that all bound variables are distinct.

Failing to that the behaviour of function bisimulation is undefined.
-}

-- The types in Example 1, page 5

-- t1 = (μx.&{N: x;x;?Int, L: ?Int}); (μz.!Int;z;z)
t1 = Semi defaultPos (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "x") (Kind defaultPos Functional Un)) (Choice defaultPos In (Map.fromList [(mkVar defaultPos "L", Message defaultPos In IntType),(mkVar defaultPos "N", Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Message defaultPos In IntType)))]))) (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "z") (Kind defaultPos Functional Un)) (Semi defaultPos (Message defaultPos Out IntType) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "z")) (TypeVar defaultPos (mkVar defaultPos "z")))))

-- t2 = (μy.&{N: y;y, L: skip}; ?Int); (μw.!Int;w)
t2 = Semi defaultPos (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "y") (Kind defaultPos Functional Un)) (Semi defaultPos (Choice defaultPos In (Map.fromList [(mkVar defaultPos "L",Skip defaultPos),(mkVar defaultPos "N",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "y")) (TypeVar defaultPos (mkVar defaultPos "y")))])) (Message defaultPos In IntType))) (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "w") (Kind defaultPos Functional Un)) (Semi defaultPos (Message defaultPos Out IntType) (TypeVar defaultPos (mkVar defaultPos "w"))))

-- The types in Section 5, page 12 (renamed, so that all bound
-- variables in the examples are distinct)

-- t3 = μu.&{Add: u;u;!Int, Const: ?Int; !Int, Mult: u;u;!Int}
t3 = Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "u") (Kind defaultPos Functional Un)) (Choice defaultPos In (Map.fromList [(mkVar defaultPos "Add",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "u")) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "u")) (Message defaultPos Out IntType))),(mkVar defaultPos "Const",Semi defaultPos (Message defaultPos In IntType) (Message defaultPos Out IntType)),(mkVar defaultPos "Mult",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "u")) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "u")) (Message defaultPos Out IntType)))]))

-- t4 = μv.&{Add: v;v, Const: ?Int, Mult: v;v}; !Int
t4 = Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "v") (Kind defaultPos Functional Un)) (Semi defaultPos (Choice defaultPos In (Map.fromList [(mkVar defaultPos "Add",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "v")) (TypeVar defaultPos (mkVar defaultPos "v"))),(mkVar defaultPos "Const",Message defaultPos In IntType),(mkVar defaultPos "Mult",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "v")) (TypeVar defaultPos (mkVar defaultPos "v")))])) (Message defaultPos Out IntType))

main = do
  putEquiv t1 t2
  putEquiv t3 t4
  putEquiv t1 t3

putEquiv :: Type -> Type -> IO ()
putEquiv t u =
  putStrLn $ "Type\n\t" ++ show t ++ "\n" ++
             "is " ++ yesNo ++ "equivalent to type\n" ++
             "\t" ++ show u ++ "\n"
    where yesNo = if bisimilar Map.empty t u then "" else "not "
  
