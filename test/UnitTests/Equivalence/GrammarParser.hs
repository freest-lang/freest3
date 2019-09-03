{-# OPTIONS_GHC -w #-}
module Equivalence.GrammarParser (
  parseGrammar
, parseGrammars  
) where

import Equivalence.GrammarLexer
import Equivalence.Grammar
import Syntax.TypeVariables
import Syntax.Base
import Syntax.Types
import Prelude hiding (Word)
import qualified Data.Map.Strict as Map
import Debug.Trace
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 ([Grammar])
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 (())
	| HappyAbsSyn8 (Grammar)
	| HappyAbsSyn9 (Productions)
	| HappyAbsSyn11 (Transitions)
	| HappyAbsSyn12 (Word)
	| HappyAbsSyn13 (Label)
	| HappyAbsSyn14 (Polarity)
	| HappyAbsSyn16 (BasicType)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,82) ([0,8,0,64,0,128,0,32,0,0,0,128,0,0,0,128,0,0,1,4096,0,0,0,0,0,32768,0,32768,0,0,0,0,2,0,1,0,32,0,256,0,512,0,64,0,128,0,256,0,128,0,8192,0,0,0,0,0,8192,0,8192,0,8192,7680,16384,0,0,0,0,0,0,2,0,14368,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_grammar","%start_grammars","Grammars","emptyNL","NL","Grammar","Productions","Production","Transition","Word","Label","PolarityMessage","PolarityChoice","BasicType","Symbol","'->'","nl","'('","')'","'{'","'}'","';'","','","Int","Bool","Char","'+'","'&'","'!'","'?'","%eof"]
        bit_start = st * 33
        bit_end = (st + 1) * 33
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..32]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (20) = happyShift action_7
action_0 (8) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (22) = happyShift action_5
action_1 (5) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (22) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (19) = happyShift action_11
action_3 (6) = happyGoto action_12
action_3 (7) = happyGoto action_10
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (33) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (19) = happyShift action_11
action_5 (6) = happyGoto action_9
action_5 (7) = happyGoto action_10
action_5 _ = happyReduce_5

action_6 (33) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (17) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (25) = happyShift action_16
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (20) = happyShift action_7
action_9 (8) = happyGoto action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_4

action_11 (19) = happyShift action_11
action_11 (7) = happyGoto action_14
action_11 _ = happyReduce_7

action_12 (20) = happyShift action_7
action_12 (8) = happyGoto action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (19) = happyShift action_11
action_13 (6) = happyGoto action_19
action_13 (7) = happyGoto action_10
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_6

action_15 (19) = happyShift action_11
action_15 (6) = happyGoto action_18
action_15 (7) = happyGoto action_10
action_15 _ = happyReduce_5

action_16 (17) = happyShift action_17
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (23) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (23) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (19) = happyShift action_11
action_20 (6) = happyGoto action_24
action_20 (7) = happyGoto action_10
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (19) = happyShift action_11
action_21 (33) = happyReduce_3
action_21 (6) = happyGoto action_24
action_21 (7) = happyGoto action_10
action_21 _ = happyReduce_5

action_22 (19) = happyShift action_11
action_22 (7) = happyGoto action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (17) = happyShift action_28
action_23 (9) = happyGoto action_26
action_23 (10) = happyGoto action_27
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (22) = happyShift action_5
action_24 (5) = happyGoto action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_2

action_26 _ = happyReduce_8

action_27 (19) = happyShift action_11
action_27 (7) = happyGoto action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (18) = happyShift action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (17) = happyShift action_36
action_29 (29) = happyShift action_37
action_29 (30) = happyShift action_38
action_29 (31) = happyShift action_39
action_29 (32) = happyShift action_40
action_29 (11) = happyGoto action_32
action_29 (13) = happyGoto action_33
action_29 (14) = happyGoto action_34
action_29 (15) = happyGoto action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (17) = happyShift action_28
action_30 (9) = happyGoto action_31
action_30 (10) = happyGoto action_27
action_30 _ = happyReduce_9

action_31 _ = happyReduce_10

action_32 _ = happyReduce_11

action_33 (17) = happyShift action_48
action_33 (12) = happyGoto action_47
action_33 _ = happyReduce_14

action_34 (20) = happyShift action_43
action_34 (26) = happyShift action_44
action_34 (27) = happyShift action_45
action_34 (28) = happyShift action_46
action_34 (16) = happyGoto action_42
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (17) = happyShift action_41
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_17

action_37 _ = happyReduce_20

action_38 _ = happyReduce_21

action_39 _ = happyReduce_18

action_40 _ = happyReduce_19

action_41 _ = happyReduce_16

action_42 _ = happyReduce_15

action_43 (21) = happyShift action_50
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_22

action_45 _ = happyReduce_23

action_46 _ = happyReduce_24

action_47 _ = happyReduce_12

action_48 (17) = happyShift action_48
action_48 (12) = happyGoto action_49
action_48 _ = happyReduce_14

action_49 _ = happyReduce_13

action_50 _ = happyReduce_25

happyReduce_2 = happyReduce 7 5 happyReduction_2
happyReduction_2 ((HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (happy_var_3 : happy_var_7
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 5 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ([happy_var_3]
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn6
		 (
	)

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 (
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 _
	_
	 =  HappyAbsSyn7
		 (
	)

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (
	)

happyReduce_8 = happyReduce 7 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Grammar [[mkVar defaultPos (show happy_var_2)], [mkVar defaultPos (show happy_var_4)]] happy_var_7
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (insertProd happy_var_3 happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (Map.singleton (mkVar defaultPos (show happy_var_1)) happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (Map.singleton happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  12 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 ((mkVar defaultPos (show happy_var_1)) : happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  12 happyReduction_14
happyReduction_14  =  HappyAbsSyn12
		 ([]
	)

happyReduce_15 = happySpecReduce_2  13 happyReduction_15
happyReduction_15 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (MessageLabel happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  13 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (ChoiceLabel happy_var_1 (mkVar defaultPos (show happy_var_2))
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (VarLabel (mkVar defaultPos (show happy_var_1))
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn14
		 (Out
	)

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn14
		 (In
	)

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn14
		 (Out
	)

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn14
		 (In
	)

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn16
		 (IntType
	)

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn16
		 (BoolType
	)

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn16
		 (CharType
	)

happyReduce_25 = happySpecReduce_2  16 happyReduction_25
happyReduction_25 _
	_
	 =  HappyAbsSyn16
		 (UnitType
	)

happyNewToken action sts stk [] =
	action 33 33 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenString _ -> cont 17;
	TokenArrow -> cont 18;
	TokenNL -> cont 19;
	TokenLParen -> cont 20;
	TokenRParen -> cont 21;
	TokenLBracket -> cont 22;
	TokenRBracket -> cont 23;
	TokenSemi -> cont 24;
	TokenComma -> cont 25;
	TokenInt -> cont 26;
	TokenBool -> cont 27;
	TokenChar -> cont 28;
	TokenPlus -> cont 29;
	TokenAmpersand -> cont 30;
	TokenExclamation -> cont 31;
	TokenQuestion -> cont 32;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 33 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
grammar tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

grammars tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseGrammar :: String -> Grammar
parseGrammar = grammar . scanTokens

parseGrammars :: String -> [Grammar]
parseGrammars = grammars . scanTokens
  -- where parse = grammars . tokens
  --       tokens s = trace ("\n\n" ++ show (scanTokens s) ++ "\n") scanTokens s
  
parseError :: [Token] -> a
parseError ts = error $ "Parse error" ++ "\n" ++ show ts

insertProd :: Productions -> Productions -> Productions
insertProd ps = Map.foldlWithKey (\acc t ts -> insert t ts acc) ps
  where
    insert :: TypeVar -> Transitions -> Productions -> Productions
    insert t ts ps =
      case ps Map.!? t of
        Just tss -> Map.insert t (Map.union ts tss) ps
        otherwise -> Map.insert t ts ps
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-8.6.3/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc21143_0/ghc_2.h" #-}




























































































































































































































































































































































































































































































































































































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
