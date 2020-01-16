{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,133) ([20992,32784,41984,32,1,63548,36865,130,8196,261,8,0,32768,1044,32,2048,0,0,0,8356,256,32768,0,30720,1009,61952,1984,57472,3969,5248,8196,0,0,0,0,0,0,0,0,36864,130,4,0,0,0,0,0,0,0,0,0,0,0,0,15360,504,0,0,1312,2049,2624,4098,0,0,0,0,0,32,0,64542,18432,65,2,0,0,0,0,33249,32783,1044,32,0,0,4178,128,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","List","ListHelper","Args","Binop","Expr","type","if","then","else","while","do","int","assign","plus","minus","times","div","lparen","rparen","pipe","colon","arrow","comma","eq","gt","lt","lte","gte","symbol","%eof"]
        bit_start = st * 33
        bit_end = (st + 1) * 33
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..32]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (10) = happyShift action_3
action_0 (13) = happyShift action_4
action_0 (15) = happyShift action_5
action_0 (21) = happyShift action_6
action_0 (32) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (8) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_3
action_1 (13) = happyShift action_4
action_1 (15) = happyShift action_5
action_1 (21) = happyShift action_6
action_1 (32) = happyShift action_7
action_1 (8) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (17) = happyShift action_15
action_2 (18) = happyShift action_16
action_2 (19) = happyShift action_17
action_2 (20) = happyShift action_18
action_2 (26) = happyShift action_19
action_2 (27) = happyShift action_20
action_2 (28) = happyShift action_21
action_2 (29) = happyShift action_22
action_2 (30) = happyShift action_23
action_2 (31) = happyShift action_24
action_2 (7) = happyGoto action_14
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (10) = happyShift action_3
action_3 (13) = happyShift action_4
action_3 (15) = happyShift action_5
action_3 (21) = happyShift action_6
action_3 (32) = happyShift action_7
action_3 (8) = happyGoto action_13
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (10) = happyShift action_3
action_4 (13) = happyShift action_4
action_4 (15) = happyShift action_5
action_4 (21) = happyShift action_6
action_4 (32) = happyShift action_7
action_4 (8) = happyGoto action_12
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_15

action_6 (10) = happyShift action_3
action_6 (13) = happyShift action_4
action_6 (15) = happyShift action_5
action_6 (21) = happyShift action_6
action_6 (32) = happyShift action_7
action_6 (4) = happyGoto action_10
action_6 (8) = happyGoto action_11
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (21) = happyShift action_9
action_7 _ = happyReduce_16

action_8 (33) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (10) = happyShift action_3
action_9 (13) = happyShift action_4
action_9 (15) = happyShift action_5
action_9 (21) = happyShift action_6
action_9 (32) = happyShift action_7
action_9 (6) = happyGoto action_32
action_9 (8) = happyGoto action_33
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (22) = happyShift action_31
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (17) = happyShift action_15
action_11 (18) = happyShift action_16
action_11 (19) = happyShift action_17
action_11 (20) = happyShift action_18
action_11 (22) = happyShift action_30
action_11 (26) = happyShift action_19
action_11 (27) = happyShift action_20
action_11 (28) = happyShift action_21
action_11 (29) = happyShift action_22
action_11 (30) = happyShift action_23
action_11 (31) = happyShift action_24
action_11 (7) = happyGoto action_14
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (14) = happyShift action_29
action_12 (17) = happyShift action_15
action_12 (18) = happyShift action_16
action_12 (19) = happyShift action_17
action_12 (20) = happyShift action_18
action_12 (27) = happyShift action_20
action_12 (28) = happyShift action_21
action_12 (29) = happyShift action_22
action_12 (30) = happyShift action_23
action_12 (31) = happyShift action_24
action_12 (7) = happyGoto action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (11) = happyShift action_28
action_13 (17) = happyShift action_15
action_13 (18) = happyShift action_16
action_13 (19) = happyShift action_17
action_13 (20) = happyShift action_18
action_13 (27) = happyShift action_20
action_13 (28) = happyShift action_21
action_13 (29) = happyShift action_22
action_13 (30) = happyShift action_23
action_13 (31) = happyShift action_24
action_13 (7) = happyGoto action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (10) = happyShift action_3
action_14 (13) = happyShift action_4
action_14 (15) = happyShift action_5
action_14 (21) = happyShift action_6
action_14 (32) = happyShift action_7
action_14 (8) = happyGoto action_27
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_6

action_16 _ = happyReduce_7

action_17 _ = happyReduce_8

action_18 _ = happyReduce_9

action_19 (10) = happyShift action_3
action_19 (13) = happyShift action_4
action_19 (15) = happyShift action_5
action_19 (21) = happyShift action_6
action_19 (32) = happyShift action_7
action_19 (5) = happyGoto action_25
action_19 (8) = happyGoto action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_10

action_21 _ = happyReduce_11

action_22 _ = happyReduce_12

action_23 _ = happyReduce_13

action_24 _ = happyReduce_14

action_25 _ = happyReduce_1

action_26 (17) = happyShift action_15
action_26 (18) = happyShift action_16
action_26 (19) = happyShift action_17
action_26 (20) = happyShift action_18
action_26 (26) = happyShift action_38
action_26 (27) = happyShift action_20
action_26 (28) = happyShift action_21
action_26 (29) = happyShift action_22
action_26 (30) = happyShift action_23
action_26 (31) = happyShift action_24
action_26 (7) = happyGoto action_14
action_26 _ = happyReduce_3

action_27 (17) = happyShift action_15
action_27 (18) = happyShift action_16
action_27 (19) = happyShift action_17
action_27 (20) = happyShift action_18
action_27 (27) = happyShift action_20
action_27 (28) = happyShift action_21
action_27 (29) = happyShift action_22
action_27 (30) = happyShift action_23
action_27 (31) = happyShift action_24
action_27 (7) = happyGoto action_14
action_27 _ = happyReduce_20

action_28 (10) = happyShift action_3
action_28 (13) = happyShift action_4
action_28 (15) = happyShift action_5
action_28 (21) = happyShift action_6
action_28 (32) = happyShift action_7
action_28 (8) = happyGoto action_37
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (10) = happyShift action_3
action_29 (13) = happyShift action_4
action_29 (15) = happyShift action_5
action_29 (21) = happyShift action_6
action_29 (32) = happyShift action_7
action_29 (8) = happyGoto action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_17

action_31 _ = happyReduce_18

action_32 (22) = happyShift action_35
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (17) = happyShift action_15
action_33 (18) = happyShift action_16
action_33 (19) = happyShift action_17
action_33 (20) = happyShift action_18
action_33 (26) = happyShift action_34
action_33 (27) = happyShift action_20
action_33 (28) = happyShift action_21
action_33 (29) = happyShift action_22
action_33 (30) = happyShift action_23
action_33 (31) = happyShift action_24
action_33 (7) = happyGoto action_14
action_33 _ = happyReduce_5

action_34 (10) = happyShift action_3
action_34 (13) = happyShift action_4
action_34 (15) = happyShift action_5
action_34 (21) = happyShift action_6
action_34 (32) = happyShift action_7
action_34 (6) = happyGoto action_41
action_34 (8) = happyGoto action_33
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_19

action_36 (17) = happyShift action_15
action_36 (18) = happyShift action_16
action_36 (19) = happyShift action_17
action_36 (20) = happyShift action_18
action_36 (27) = happyShift action_20
action_36 (28) = happyShift action_21
action_36 (29) = happyShift action_22
action_36 (30) = happyShift action_23
action_36 (31) = happyShift action_24
action_36 (7) = happyGoto action_14
action_36 _ = happyReduce_22

action_37 (12) = happyShift action_40
action_37 (17) = happyShift action_15
action_37 (18) = happyShift action_16
action_37 (19) = happyShift action_17
action_37 (20) = happyShift action_18
action_37 (27) = happyShift action_20
action_37 (28) = happyShift action_21
action_37 (29) = happyShift action_22
action_37 (30) = happyShift action_23
action_37 (31) = happyShift action_24
action_37 (7) = happyGoto action_14
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (10) = happyShift action_3
action_38 (13) = happyShift action_4
action_38 (15) = happyShift action_5
action_38 (21) = happyShift action_6
action_38 (32) = happyShift action_7
action_38 (5) = happyGoto action_39
action_38 (8) = happyGoto action_26
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_2

action_40 (10) = happyShift action_3
action_40 (13) = happyShift action_4
action_40 (15) = happyShift action_5
action_40 (21) = happyShift action_6
action_40 (32) = happyShift action_7
action_40 (8) = happyGoto action_42
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_4

action_42 (17) = happyShift action_15
action_42 (18) = happyShift action_16
action_42 (19) = happyShift action_17
action_42 (20) = happyShift action_18
action_42 (27) = happyShift action_20
action_42 (28) = happyShift action_21
action_42 (29) = happyShift action_22
action_42 (30) = happyShift action_23
action_42 (31) = happyShift action_24
action_42 (7) = happyGoto action_14
action_42 _ = happyReduce_21

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (Plus
	)

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (Minus
	)

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (Times
	)

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (Div
	)

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (EqualTo
	)

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn7
		 (GreaterThan
	)

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn7
		 (LessThan
	)

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn7
		 (LessThanEqual
	)

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn7
		 (GreaterThanEqual
	)

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn8
		 (EInt happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn8
		 (ESymbol happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  8 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Paren happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  8 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Tuple happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 8 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (FunctionApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Infix happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 6 8 happyReduction_21
happyReduction_21 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Conditional happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 8 happyReduction_22
happyReduction_22 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 33 33 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenType -> cont 9;
	TokenIf -> cont 10;
	TokenThen -> cont 11;
	TokenElse -> cont 12;
	TokenWhile -> cont 13;
	TokenDo -> cont 14;
	TokenInt happy_dollar_dollar -> cont 15;
	TokenAssign -> cont 16;
	TokenPlus -> cont 17;
	TokenMinus -> cont 18;
	TokenTimes -> cont 19;
	TokenDiv -> cont 20;
	TokenLParen -> cont 21;
	TokenRParen -> cont 22;
	TokenPipe -> cont 23;
	TokenColon -> cont 24;
	TokenArrow -> cont 25;
	TokenComa -> cont 26;
	TokenEQ -> cont 27;
	TokenGT -> cont 28;
	TokenLT -> cont 29;
	TokenLTE -> cont 30;
	TokenGTE -> cont 31;
	TokenSym happy_dollar_dollar -> cont 32;
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
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Valdef = Valdef Signature [Equation]
data Signature = Signature String Type
data Equation = Equation String Expr

data Binop = Plus
           | Minus
           | Times
           | Div
           | EqualTo
           | LessThan
           | GreaterThan
           | LessThanEqual
           | GreaterThanEqual
           deriving Show

data Expr = EInt Int
          | ESymbol String
          | Paren Expr
          | Tuple [Expr]
          | FunctionApp String [Expr]
          | Infix Expr Binop Expr
          | Conditional Expr Expr Expr
          | While Expr Expr
          | Empty
          deriving Show

data Btype = Bool String
           | TInt String
           | TSymbol String
           deriving Show

data Xtype = Xtype Btype String
           deriving Show

data Ttype = Ttype [Xtype]
            deriving Show

data Ptype = Xtype' Xtype
           | Ttype' Ttype
           deriving Show

data Ftype = Ftype Ptype Ptype
           deriving Show

data Type = Ptype' Ptype
          | Ftype' Ftype
          deriving Show
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
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
