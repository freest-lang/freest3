-- Target Haskell code

import FreeSTRuntime

data Nat = Succ Nat | Zero  deriving Show


_main = ((lessThan number2) (Succ (Succ (Succ number2))))

lessThan = (\x -> (\y -> case y of {
    Succ b -> case x of {
    Succ a -> ((lessThan a) b);
    Zero -> True;};
    Zero -> False;}))

number2 = (Succ (Succ Zero))



main = putStrLn (show _main)

