{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,174) ([0,152,1024,32768,9,64,0,0,4,2432,16384,0,0,1024,0,2080,64,0,130,4,0,64,0,0,0,0,2048,64,0,57465,3,0,0,0,33280,1024,0,2048,0,8192,57464,3,16384,0,0,0,0,0,0,0,0,0,0,0,8,0,33280,1024,0,0,0,0,130,4,8192,16392,0,33280,1024,0,2080,64,0,130,4,8192,16392,0,33280,1024,0,2080,64,0,130,4,8192,16392,0,33280,1024,0,2080,64,0,256,0,32768,16151,0,33280,1024,0,8192,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,16384,0,0,0,0,0,1,0,256,0,0,16392,0,0,1024,0,1920,62,0,0,0,8192,16392,0,0,0,0,4096,0,0,61560,3,32768,7,0,30720,0,0,1920,0,0,120,0,32768,7,0,0,0,0,0,0,0,96,0,0,6,0,30784,992,0,1920,62,0,256,0,0,512,0,33280,1024,0,2080,64,0,0,0,0,0,0,30720,1008,0,0,0,0,0,0,0,0,0,0,1024,0,0,1,0,0,0,8192,16392,0,0,0,0,1920,62,0,130,4,32768,15879,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Stmts","List","ListHelper","Args","Btype","Xtype","Ttype1","Ttype","Ptype","Ftype","Type","Signature","Equation","Equations","Stmt","Expr","type","if","then","else","while","do","int","assign","plus","minus","times","div","lparen","rparen","pipe","colon","arrow","comma","eq","gt","lt","lte","gte","symbol","%eof"]
        bit_start = st * 44
        bit_end = (st + 1) * 44
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..43]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (20) = happyShift action_4
action_0 (21) = happyShift action_5
action_0 (24) = happyShift action_6
action_0 (43) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (15) = happyGoto action_2
action_0 (18) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (20) = happyShift action_4
action_1 (21) = happyShift action_5
action_1 (24) = happyShift action_6
action_1 (43) = happyShift action_7
action_1 (15) = happyGoto action_2
action_1 (18) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (43) = happyShift action_19
action_2 (16) = happyGoto action_17
action_2 (17) = happyGoto action_18
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (20) = happyShift action_4
action_3 (21) = happyShift action_5
action_3 (24) = happyShift action_6
action_3 (43) = happyShift action_7
action_3 (4) = happyGoto action_16
action_3 (15) = happyGoto action_2
action_3 (18) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (43) = happyShift action_15
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (26) = happyShift action_11
action_5 (32) = happyShift action_12
action_5 (43) = happyShift action_13
action_5 (19) = happyGoto action_14
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (26) = happyShift action_11
action_6 (32) = happyShift action_12
action_6 (43) = happyShift action_13
action_6 (19) = happyGoto action_10
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (35) = happyShift action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (44) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (32) = happyShift action_43
action_9 (43) = happyShift action_44
action_9 (8) = happyGoto action_37
action_9 (9) = happyGoto action_38
action_9 (11) = happyGoto action_39
action_9 (12) = happyGoto action_40
action_9 (13) = happyGoto action_41
action_9 (14) = happyGoto action_42
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (25) = happyShift action_36
action_10 (28) = happyShift action_24
action_10 (29) = happyShift action_25
action_10 (30) = happyShift action_26
action_10 (31) = happyShift action_27
action_10 (38) = happyShift action_28
action_10 (39) = happyShift action_29
action_10 (40) = happyShift action_30
action_10 (41) = happyShift action_31
action_10 (42) = happyShift action_32
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_27

action_12 (26) = happyShift action_11
action_12 (32) = happyShift action_12
action_12 (43) = happyShift action_13
action_12 (5) = happyGoto action_34
action_12 (19) = happyGoto action_35
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (32) = happyShift action_33
action_13 _ = happyReduce_28

action_14 (22) = happyShift action_23
action_14 (28) = happyShift action_24
action_14 (29) = happyShift action_25
action_14 (30) = happyShift action_26
action_14 (31) = happyShift action_27
action_14 (38) = happyShift action_28
action_14 (39) = happyShift action_29
action_14 (40) = happyShift action_30
action_14 (41) = happyShift action_31
action_14 (42) = happyShift action_32
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (27) = happyShift action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_1

action_17 (43) = happyShift action_19
action_17 (16) = happyGoto action_17
action_17 (17) = happyGoto action_21
action_17 _ = happyReduce_22

action_18 _ = happyReduce_25

action_19 (32) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (26) = happyShift action_11
action_20 (32) = happyShift action_12
action_20 (43) = happyShift action_13
action_20 (7) = happyGoto action_66
action_20 (19) = happyGoto action_54
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_21

action_22 (26) = happyShift action_11
action_22 (32) = happyShift action_12
action_22 (43) = happyShift action_13
action_22 (19) = happyGoto action_65
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (26) = happyShift action_11
action_23 (32) = happyShift action_12
action_23 (43) = happyShift action_13
action_23 (19) = happyGoto action_64
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (26) = happyShift action_11
action_24 (32) = happyShift action_12
action_24 (43) = happyShift action_13
action_24 (19) = happyGoto action_63
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (26) = happyShift action_11
action_25 (32) = happyShift action_12
action_25 (43) = happyShift action_13
action_25 (19) = happyGoto action_62
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (26) = happyShift action_11
action_26 (32) = happyShift action_12
action_26 (43) = happyShift action_13
action_26 (19) = happyGoto action_61
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (26) = happyShift action_11
action_27 (32) = happyShift action_12
action_27 (43) = happyShift action_13
action_27 (19) = happyGoto action_60
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (26) = happyShift action_11
action_28 (32) = happyShift action_12
action_28 (43) = happyShift action_13
action_28 (19) = happyGoto action_59
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (26) = happyShift action_11
action_29 (32) = happyShift action_12
action_29 (43) = happyShift action_13
action_29 (19) = happyGoto action_58
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (26) = happyShift action_11
action_30 (32) = happyShift action_12
action_30 (43) = happyShift action_13
action_30 (19) = happyGoto action_57
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (26) = happyShift action_11
action_31 (32) = happyShift action_12
action_31 (43) = happyShift action_13
action_31 (19) = happyGoto action_56
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (26) = happyShift action_11
action_32 (32) = happyShift action_12
action_32 (43) = happyShift action_13
action_32 (19) = happyGoto action_55
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (26) = happyShift action_11
action_33 (32) = happyShift action_12
action_33 (43) = happyShift action_13
action_33 (7) = happyGoto action_53
action_33 (19) = happyGoto action_54
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (33) = happyShift action_52
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (28) = happyShift action_24
action_35 (29) = happyShift action_25
action_35 (30) = happyShift action_26
action_35 (31) = happyShift action_27
action_35 (33) = happyShift action_50
action_35 (37) = happyShift action_51
action_35 (38) = happyShift action_28
action_35 (39) = happyShift action_29
action_35 (40) = happyShift action_30
action_35 (41) = happyShift action_31
action_35 (42) = happyShift action_32
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (26) = happyShift action_11
action_36 (32) = happyShift action_12
action_36 (43) = happyShift action_13
action_36 (19) = happyGoto action_49
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (34) = happyShift action_48
action_37 _ = happyReduce_10

action_38 _ = happyReduce_15

action_39 _ = happyReduce_14

action_40 (36) = happyShift action_47
action_40 _ = happyReduce_17

action_41 _ = happyReduce_18

action_42 _ = happyReduce_19

action_43 (43) = happyShift action_44
action_43 (8) = happyGoto action_37
action_43 (9) = happyGoto action_45
action_43 (10) = happyGoto action_46
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_8

action_45 (37) = happyShift action_76
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (33) = happyShift action_75
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (32) = happyShift action_43
action_47 (43) = happyShift action_44
action_47 (8) = happyGoto action_37
action_47 (9) = happyGoto action_38
action_47 (11) = happyGoto action_39
action_47 (12) = happyGoto action_74
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (43) = happyShift action_44
action_48 (8) = happyGoto action_37
action_48 (9) = happyGoto action_73
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (28) = happyShift action_24
action_49 (29) = happyShift action_25
action_49 (30) = happyShift action_26
action_49 (31) = happyShift action_27
action_49 (38) = happyShift action_28
action_49 (39) = happyShift action_29
action_49 (40) = happyShift action_30
action_49 (41) = happyShift action_31
action_49 (42) = happyShift action_32
action_49 _ = happyReduce_24

action_50 _ = happyReduce_29

action_51 (26) = happyShift action_11
action_51 (32) = happyShift action_12
action_51 (43) = happyShift action_13
action_51 (6) = happyGoto action_71
action_51 (19) = happyGoto action_72
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_30

action_53 (33) = happyShift action_70
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (28) = happyShift action_24
action_54 (29) = happyShift action_25
action_54 (30) = happyShift action_26
action_54 (31) = happyShift action_27
action_54 (37) = happyShift action_69
action_54 (38) = happyShift action_28
action_54 (39) = happyShift action_29
action_54 (40) = happyShift action_30
action_54 (41) = happyShift action_31
action_54 (42) = happyShift action_32
action_54 _ = happyReduce_7

action_55 (28) = happyShift action_24
action_55 (29) = happyShift action_25
action_55 (30) = happyShift action_26
action_55 (31) = happyShift action_27
action_55 _ = happyReduce_40

action_56 (28) = happyShift action_24
action_56 (29) = happyShift action_25
action_56 (30) = happyShift action_26
action_56 (31) = happyShift action_27
action_56 _ = happyReduce_39

action_57 (28) = happyShift action_24
action_57 (29) = happyShift action_25
action_57 (30) = happyShift action_26
action_57 (31) = happyShift action_27
action_57 _ = happyReduce_38

action_58 (28) = happyShift action_24
action_58 (29) = happyShift action_25
action_58 (30) = happyShift action_26
action_58 (31) = happyShift action_27
action_58 _ = happyReduce_37

action_59 (28) = happyShift action_24
action_59 (29) = happyShift action_25
action_59 (30) = happyShift action_26
action_59 (31) = happyShift action_27
action_59 _ = happyReduce_36

action_60 _ = happyReduce_35

action_61 _ = happyReduce_34

action_62 (30) = happyShift action_26
action_62 (31) = happyShift action_27
action_62 _ = happyReduce_33

action_63 (30) = happyShift action_26
action_63 (31) = happyShift action_27
action_63 _ = happyReduce_32

action_64 (23) = happyShift action_68
action_64 (28) = happyShift action_24
action_64 (29) = happyShift action_25
action_64 (30) = happyShift action_26
action_64 (31) = happyShift action_27
action_64 (38) = happyShift action_28
action_64 (39) = happyShift action_29
action_64 (40) = happyShift action_30
action_64 (41) = happyShift action_31
action_64 (42) = happyShift action_32
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (28) = happyShift action_24
action_65 (29) = happyShift action_25
action_65 (30) = happyShift action_26
action_65 (31) = happyShift action_27
action_65 (38) = happyShift action_28
action_65 (39) = happyShift action_29
action_65 (40) = happyShift action_30
action_65 (41) = happyShift action_31
action_65 (42) = happyShift action_32
action_65 _ = happyReduce_26

action_66 (33) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (38) = happyShift action_82
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (26) = happyShift action_11
action_68 (32) = happyShift action_12
action_68 (43) = happyShift action_13
action_68 (19) = happyGoto action_81
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (26) = happyShift action_11
action_69 (32) = happyShift action_12
action_69 (43) = happyShift action_13
action_69 (7) = happyGoto action_80
action_69 (19) = happyGoto action_54
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_31

action_71 _ = happyReduce_3

action_72 (28) = happyShift action_24
action_72 (29) = happyShift action_25
action_72 (30) = happyShift action_26
action_72 (31) = happyShift action_27
action_72 (37) = happyShift action_79
action_72 (38) = happyShift action_28
action_72 (39) = happyShift action_29
action_72 (40) = happyShift action_30
action_72 (41) = happyShift action_31
action_72 (42) = happyShift action_32
action_72 _ = happyReduce_5

action_73 _ = happyReduce_9

action_74 _ = happyReduce_16

action_75 _ = happyReduce_13

action_76 (43) = happyShift action_44
action_76 (8) = happyGoto action_37
action_76 (9) = happyGoto action_77
action_76 (10) = happyGoto action_78
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (37) = happyShift action_76
action_77 _ = happyReduce_12

action_78 _ = happyReduce_11

action_79 (26) = happyShift action_11
action_79 (32) = happyShift action_12
action_79 (43) = happyShift action_13
action_79 (6) = happyGoto action_84
action_79 (19) = happyGoto action_72
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_6

action_81 (28) = happyShift action_24
action_81 (29) = happyShift action_25
action_81 (30) = happyShift action_26
action_81 (31) = happyShift action_27
action_81 (38) = happyShift action_28
action_81 (39) = happyShift action_29
action_81 (40) = happyShift action_30
action_81 (41) = happyShift action_31
action_81 (42) = happyShift action_32
action_81 _ = happyReduce_23

action_82 (26) = happyShift action_11
action_82 (32) = happyShift action_12
action_82 (43) = happyShift action_13
action_82 (19) = happyGoto action_83
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (28) = happyShift action_24
action_83 (29) = happyShift action_25
action_83 (30) = happyShift action_26
action_83 (31) = happyShift action_27
action_83 (38) = happyShift action_28
action_83 (39) = happyShift action_29
action_83 (40) = happyShift action_30
action_83 (41) = happyShift action_31
action_83 (42) = happyShift action_32
action_83 _ = happyReduce_20

action_84 _ = happyReduce_4

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1] ++ happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  4 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn8
		 (Btype happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 (let Xtype l = happy_var_3 in Xtype $ [happy_var_1] ++ l
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 (Xtype [happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (let Ttype l = happy_var_3 in Ttype $ [happy_var_1] ++ l
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (Ttype $ [happy_var_1] ++ [happy_var_3]
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (Ttype' happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn12
		 (Xtype' happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 (Ftype happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn14
		 (Ptype' happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (Ftype' happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn15
		 (Signature happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 16 happyReduction_20
happyReduction_20 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Equation happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  17 happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1] ++ happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  17 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 6 18 happyReduction_23
happyReduction_23 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Conditional happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 18 happyReduction_24
happyReduction_24 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_2  18 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (Valdef happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 18 happyReduction_26
happyReduction_26 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Typedef happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  19 happyReduction_27
happyReduction_27 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn19
		 (EInt happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn19
		 (ESymbol happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  19 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Paren happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  19 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Tuple happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happyReduce 4 19 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (FunctionApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  19 happyReduction_32
happyReduction_32 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 Plus happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  19 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 Minus happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 Times happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  19 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 Div happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 EqualTo happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  19 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 GreaterThan happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 LessThan happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  19 happyReduction_39
happyReduction_39 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 LessThanEqual happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  19 happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Infix happy_var_1 GreaterThanEqual happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 44 44 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenType -> cont 20;
	TokenIf -> cont 21;
	TokenThen -> cont 22;
	TokenElse -> cont 23;
	TokenWhile -> cont 24;
	TokenDo -> cont 25;
	TokenInt happy_dollar_dollar -> cont 26;
	TokenAssign -> cont 27;
	TokenPlus -> cont 28;
	TokenMinus -> cont 29;
	TokenTimes -> cont 30;
	TokenDiv -> cont 31;
	TokenLParen -> cont 32;
	TokenRParen -> cont 33;
	TokenPipe -> cont 34;
	TokenColon -> cont 35;
	TokenArrow -> cont 36;
	TokenComa -> cont 37;
	TokenEQ -> cont 38;
	TokenGT -> cont 39;
	TokenLT -> cont 40;
	TokenLTE -> cont 41;
	TokenGTE -> cont 42;
	TokenSym happy_dollar_dollar -> cont 43;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 44 tk tks = happyError' (tks, explist)
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

data Signature = Signature String Type
               deriving Show
data Equation = Equation String [Expr] Expr
              deriving Show

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

data Stmt = Conditional Expr Expr Expr
          | While Expr Expr
          | Valdef Signature [Equation]
          | Typedef String Expr
          deriving Show

data Expr = EInt Int
          | ESymbol String
          | Paren Expr
          | Tuple [Expr]
          | FunctionApp String [Expr]
          | Infix Expr Binop Expr
          | Empty
          deriving Show

data Btype = Btype String
           deriving Show

data Xtype = Xtype [Btype]
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
