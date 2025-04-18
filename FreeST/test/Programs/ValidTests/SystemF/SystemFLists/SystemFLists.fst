{- |
Module      : SystemFLists
Description : Pairs in System F
Copyright   : (c) Vasco T. Vasconcelos, Gil Silva, 2 Dec 2021

Church Encoding _ Lists
-}

type List a = ∀r . (a -> r -> r) -> r -> r

-- The empty list constructor
nil : ∀a r . (a -> r -> r) -> r -> r
nil c n = n
-- nil = Λa r => λc:(a -> r -> r) n:r -> n -- extended version

-- The cons list constructor
cons : ∀a . a -> (∀r . (a -> r -> r) -> r -> r) -> (∀r . (a -> r -> r) -> r -> r)
cons hd tl c n = c hd (tl  @r c n)
-- cons = Λa => λhd:a -> λtl:(∀r . (a -> r -> r) -> r -> r) ->
--          Λr => λc:(a -> r -> r) -> λn:r -> c hd (tl [r] c n) -- extended version

-- Some lists
empty', singleton', twoChars : ∀r . (Char -> r -> r) -> r -> r
empty' = nil  @Char

singleton' = cons  @Char 'a' empty'

twoChars = cons  @Char 'b' singleton'

diverge' : ∀a . () -> a
diverge' x = diverge' @a x

-- Function head takes the head of a non-empty list and diverges otherwise
head' : ∀a . (∀r . (a -> r -> r) -> r -> r) -> a
head' l = (l  @(() -> a) (λhd:a tl:(()->a) _:() -> hd) (diverge' @a)) ()
-- head = Λa => λl:(∀r . (a -> r -> r) -> r -> r) ->
--   (l [()->a] (λhd:a tl:(()->a) _:() -> hd) (diverge [a])) () -- extended version

-- The null predicate: is the list empty?
null' : ∀a . (∀r . (a -> r -> r) -> r -> r) -> Bool
null' l = l  @Bool (λhd:a tl:Bool -> False) True
-- null = Λa => λl:(∀r . (a -> r -> r) -> r -> r) -> l [Bool] (λhd:a tl:Bool -> False) True -- extended version

mainNull : Bool
mainNull = null'  @Char twoChars

mainHead : Char
mainHead = head'  @Char twoChars

mainChars : Char
mainChars = head'  @Char twoChars -- null'  @Char (nil  @Char)

-- Pairs in preparation for the tail function

type Pair = ∀ a b . (∀ c . (a -> b -> c) -> c)

pair : ∀ a b . a -> b -> (∀ c . (a -> b -> c) -> c)
pair x y = Λc=> λf:(a->b->c)-> f x y

fst' : ∀ a b . (∀ c . (a -> b -> c) -> c) -> a
fst' p = p @a (λf:a-> λs:b-> f)

snd' : ∀ a b . (∀ c . (a -> b -> c) -> c) -> b
snd' p = p @b (λf:a-> λs:b-> s)

-- Function tail takes the tail of a non-empty list.
tail' : ∀ a . (∀ b . (a -> b -> b) -> b -> b) -> (∀ b . (a -> b -> b) -> b -> b)
tail' l = (fst' @(∀b . (a -> b -> b) -> b -> b) @(∀b . (a -> b -> b) -> b -> b) (
            l @(∀c . ((∀b . (a -> b -> b) -> b -> b) -> (∀b . (a -> b -> b) -> b -> b) -> c) -> c)
              (λh:a-> λt:(∀c.((∀b.(a->b->b)->b->b)->(∀b.(a->b->b)->b->b)->c)->c)->
                pair @(∀b . (a -> b -> b) -> b -> b) @(∀b . (a -> b -> b) -> b -> b)
                  (snd' @(∀b . (a -> b -> b) -> b -> b) @(∀b . (a -> b -> b) -> b -> b) t)
                  (cons @a h (snd' @(∀b . (a -> b -> b) -> b -> b) @(∀b . (a -> b -> b) -> b -> b) t)))
              (pair @(∀b . (a -> b -> b) -> b -> b) @(∀b . (a -> b -> b) -> b -> b) (nil @a) (nil @a))))

mainTail : Char
mainTail = head'  @Char $ tail'  @Char twoChars

-- The length of a list, given as a primitive Int
length' : ∀a . (∀r . (a -> r -> r) -> r -> r) -> Int
length' l = l  @Int (λ_:a -> succ) 0

mainLength : Int
mainLength = length'  @Char twoChars

-- Natural numbers
type Nat = ∀ a . (a -> a) -> a -> a

-- Some nats
zero, one, four : Nat
zero _ z = z

one s z = s z

four s z = s $ s $ s $ s z

-- replicate n x is a list of length n with x the value of every element
replicate : ∀ x . Nat -> x -> (∀ r . (x -> r -> r) -> r -> r)
replicate n val = n @(∀r . (x -> r -> r) -> r -> r) (cons @x val) (nil @x)

main : Int
main = length'  @Char $ replicate  @Char four 'a'
