import           Syntax.Types
import           Syntax.Base
import           Syntax.Kinds
import Equivalence.Equivalence
import qualified Data.Map.Strict as Map

-- Example 1

-- t1 = (μx.&{n : x; x; ?int, l :?int}); (μz.!int; z; z)
t1 = Semi defaultPos (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "x") (Kind defaultPos Functional Un)) (Choice defaultPos In (Map.fromList [(mkVar defaultPos "L", Message defaultPos In IntType),(mkVar defaultPos "N", Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Message defaultPos In IntType)))]))) (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "z") (Kind defaultPos Functional Un)) (Semi defaultPos (Message defaultPos Out IntType) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "z")) (TypeVar defaultPos (mkVar defaultPos "z")))))

-- t2 = (μy.&{n : y; y, l : skip}; ?int); (μw.!int; w)
t2 = Semi defaultPos (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "y") (Kind defaultPos Functional Un)) (Semi defaultPos (Choice defaultPos In (Map.fromList [(mkVar defaultPos "L",Skip defaultPos),(mkVar defaultPos "N",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "y")) (TypeVar defaultPos (mkVar defaultPos "y")))])) (Message defaultPos In IntType))) (Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "w") (Kind defaultPos Functional Un)) (Semi defaultPos (Message defaultPos Out IntType) (TypeVar defaultPos (mkVar defaultPos "w"))))

-- Example 2

-- t3 = μx.&{Add : x; x; ! int, Const : ? int; !int, Mult : x; x; ! int}
t3 = Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "x") (Kind defaultPos Functional Un)) (Choice defaultPos In (Map.fromList [(mkVar defaultPos "Add",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Message defaultPos Out IntType))),(mkVar defaultPos "Const",Semi defaultPos (Message defaultPos In IntType) (Message defaultPos Out IntType)),(mkVar defaultPos "Mult",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (Message defaultPos Out IntType)))]))

-- t4 = μx.&{Add : x; x, Const : ? int, Mult : x; x}; ! int
t4 = Rec defaultPos (TypeVarBind defaultPos (mkVar defaultPos "x") (Kind defaultPos Functional Un)) (Semi defaultPos (Choice defaultPos In (Map.fromList [(mkVar defaultPos "Add",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (TypeVar defaultPos (mkVar defaultPos "x"))),(mkVar defaultPos "Const",Message defaultPos In IntType),(mkVar defaultPos "Mult",Semi defaultPos (TypeVar defaultPos (mkVar defaultPos "x")) (TypeVar defaultPos (mkVar defaultPos "x")))])) (Message defaultPos Out IntType))


main = do
  print $ equivalent Map.empty Map.empty t1 t2
  print $ equivalent Map.empty Map.empty t3 t4
