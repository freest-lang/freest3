Addition of:
- Guards
- Data type patterns
- Channel patterns

### Guards (functions and cases)
```haskell
f : Int -> Int
f x
  | x == 0    =  50
  | x == 1    =  00
  | otherwise = -50

data T = A Int | B

g : T -> Int
g t =
  case t of {
    A x | x == 0    -> 10
        | x == 1    -> 20
        | otherwise -> 30
  }
```

### Data type patterns (functions and cases)
```haskell
data T = A T | B

f : T -> Int
f (A B) = 0
f _     = 1

g : T -> Int
f x =
  case x of {
    A B -> 0,
    _   -> 1
  }
```

### Channel patterns (functions and cases)
```haskell
type C = &{A: &{C: Skip, D: Skip}, B: Skip}

f : C -> Int
f (A (C c)) = 0
f (A (D c)) = 1
f (B c)     = 2

```