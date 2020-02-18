{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import Ast
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23
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
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,260) ([0,128,0,2,1024,0,16,0,0,64,0,1,1024,0,0,4096,0,0,32800,0,0,0,0,4096,0,64,0,0,0,0,0,8,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,16,0,0,0,64,0,0,0,0,0,4,0,0,1,0,0,260,4,0,0,0,0,16384,256,0,0,32,0,0,32,0,0,128,2,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,8,0,0,32768,0,0,0,8,0,0,576,0,0,0,0,0,0,0,0,16640,256,0,0,2,0,21056,16400,0,0,0,0,0,33728,15,0,0,0,0,1024,1025,0,8192,8200,0,0,0,1,0,520,8,0,0,0,0,33280,512,0,0,8,0,0,512,0,0,0,0,0,2080,32,0,0,0,0,0,2050,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,128,0,0,9216,0,0,12032,62,0,0,8,0,8192,0,0,2048,31774,0,2048,57584,3,0,2080,32,0,16640,256,0,2048,2050,0,16384,16400,0,0,130,2,0,1040,16,0,8320,128,0,1024,1025,0,8192,8200,0,0,60,0,0,480,0,0,3840,0,0,30720,0,0,49152,3,0,0,0,0,0,0,0,0,1536,0,0,12288,0,0,2632,2050,0,16384,16400,0,0,130,2,0,0,0,0,0,0,0,0,0,0,8192,8200,0,0,0,0,0,49636,7,0,3840,62,0,8,0,0,5264,4100,0,42112,32800,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Stmts","OptionalArgs","OptionalList","TupleList","List","Btype","Xtype","Ttype1","Ttype","Ptype","Ftype","Type","Signature","Equation","Equations","Stmt","WeakStmt","Expr","Variable","FunctionApp","type","if","then","else","while","do","of","let","in","int","assign","plus","minus","times","div","lparen","rparen","pipe","arrow","comma","eq","gt","lt","lte","gte","symbol","functionDef","%eof"]
        bit_start = st * 51
        bit_end = (st + 1) * 51
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..50]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (24) = happyShift action_4
action_0 (50) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (16) = happyGoto action_2
action_0 (19) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (24) = happyShift action_4
action_1 (50) = happyShift action_5
action_1 (16) = happyGoto action_2
action_1 (19) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (49) = happyShift action_20
action_2 (17) = happyGoto action_18
action_2 (18) = happyGoto action_19
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (24) = happyShift action_4
action_3 (50) = happyShift action_5
action_3 (4) = happyGoto action_8
action_3 (16) = happyGoto action_2
action_3 (19) = happyGoto action_7
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (49) = happyShift action_17
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (39) = happyShift action_15
action_5 (49) = happyShift action_16
action_5 (9) = happyGoto action_9
action_5 (10) = happyGoto action_10
action_5 (12) = happyGoto action_11
action_5 (13) = happyGoto action_12
action_5 (14) = happyGoto action_13
action_5 (15) = happyGoto action_14
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (51) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (24) = happyShift action_4
action_7 (50) = happyShift action_5
action_7 (4) = happyGoto action_8
action_7 (16) = happyGoto action_2
action_7 (19) = happyGoto action_7
action_7 _ = happyReduce_2

action_8 _ = happyReduce_1

action_9 (41) = happyShift action_28
action_9 _ = happyReduce_13

action_10 _ = happyReduce_18

action_11 _ = happyReduce_17

action_12 (42) = happyShift action_27
action_12 _ = happyReduce_20

action_13 _ = happyReduce_21

action_14 _ = happyReduce_22

action_15 (49) = happyShift action_16
action_15 (9) = happyGoto action_9
action_15 (10) = happyGoto action_25
action_15 (11) = happyGoto action_26
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_11

action_17 (34) = happyShift action_24
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (49) = happyShift action_20
action_18 (17) = happyGoto action_18
action_18 (18) = happyGoto action_23
action_18 _ = happyReduce_25

action_19 _ = happyReduce_26

action_20 (39) = happyShift action_22
action_20 (6) = happyGoto action_21
action_20 _ = happyReduce_7

action_21 (34) = happyShift action_42
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (33) = happyShift action_39
action_22 (39) = happyShift action_40
action_22 (49) = happyShift action_41
action_22 (7) = happyGoto action_36
action_22 (22) = happyGoto action_37
action_22 (23) = happyGoto action_38
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_24

action_24 (39) = happyShift action_15
action_24 (49) = happyShift action_35
action_24 (9) = happyGoto action_9
action_24 (10) = happyGoto action_10
action_24 (12) = happyGoto action_11
action_24 (13) = happyGoto action_12
action_24 (14) = happyGoto action_13
action_24 (15) = happyGoto action_33
action_24 (23) = happyGoto action_34
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (43) = happyShift action_32
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (40) = happyShift action_31
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (39) = happyShift action_15
action_27 (49) = happyShift action_16
action_27 (9) = happyGoto action_9
action_27 (10) = happyGoto action_10
action_27 (12) = happyGoto action_11
action_27 (13) = happyGoto action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (49) = happyShift action_16
action_28 (9) = happyGoto action_9
action_28 (10) = happyGoto action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_12

action_30 _ = happyReduce_19

action_31 _ = happyReduce_16

action_32 (49) = happyShift action_16
action_32 (9) = happyGoto action_9
action_32 (10) = happyGoto action_58
action_32 (11) = happyGoto action_59
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_27

action_34 (30) = happyShift action_57
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (39) = happyShift action_51
action_35 (5) = happyGoto action_50
action_35 _ = happyReduce_11

action_36 (40) = happyShift action_56
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (40) = happyShift action_54
action_37 (43) = happyShift action_55
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_46

action_39 _ = happyReduce_44

action_40 (33) = happyShift action_39
action_40 (39) = happyShift action_40
action_40 (49) = happyShift action_41
action_40 (7) = happyGoto action_52
action_40 (22) = happyGoto action_53
action_40 (23) = happyGoto action_38
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (39) = happyShift action_51
action_41 (5) = happyGoto action_50
action_41 _ = happyReduce_45

action_42 (25) = happyShift action_46
action_42 (28) = happyShift action_47
action_42 (31) = happyShift action_48
action_42 (33) = happyShift action_39
action_42 (39) = happyShift action_49
action_42 (49) = happyShift action_41
action_42 (20) = happyGoto action_43
action_42 (21) = happyGoto action_44
action_42 (22) = happyGoto action_45
action_42 (23) = happyGoto action_38
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_23

action_44 (35) = happyShift action_71
action_44 (36) = happyShift action_72
action_44 (37) = happyShift action_73
action_44 (38) = happyShift action_74
action_44 (44) = happyShift action_75
action_44 (45) = happyShift action_76
action_44 (46) = happyShift action_77
action_44 (47) = happyShift action_78
action_44 (48) = happyShift action_79
action_44 _ = happyReduce_32

action_45 _ = happyReduce_33

action_46 (33) = happyShift action_39
action_46 (39) = happyShift action_49
action_46 (49) = happyShift action_41
action_46 (21) = happyGoto action_70
action_46 (22) = happyGoto action_45
action_46 (23) = happyGoto action_38
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (33) = happyShift action_39
action_47 (39) = happyShift action_49
action_47 (49) = happyShift action_41
action_47 (21) = happyGoto action_69
action_47 (22) = happyGoto action_45
action_47 (23) = happyGoto action_38
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (49) = happyShift action_68
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (33) = happyShift action_39
action_49 (39) = happyShift action_49
action_49 (49) = happyShift action_41
action_49 (7) = happyGoto action_52
action_49 (21) = happyGoto action_66
action_49 (22) = happyGoto action_67
action_49 (23) = happyGoto action_38
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_48

action_51 (33) = happyShift action_39
action_51 (39) = happyShift action_40
action_51 (49) = happyShift action_41
action_51 (7) = happyGoto action_64
action_51 (22) = happyGoto action_65
action_51 (23) = happyGoto action_38
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (40) = happyShift action_63
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (43) = happyShift action_55
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_6

action_55 (33) = happyShift action_39
action_55 (39) = happyShift action_40
action_55 (49) = happyShift action_41
action_55 (8) = happyGoto action_61
action_55 (22) = happyGoto action_62
action_55 (23) = happyGoto action_38
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_5

action_57 (39) = happyShift action_15
action_57 (49) = happyShift action_16
action_57 (9) = happyGoto action_9
action_57 (10) = happyGoto action_10
action_57 (12) = happyGoto action_11
action_57 (13) = happyGoto action_12
action_57 (14) = happyGoto action_13
action_57 (15) = happyGoto action_60
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (43) = happyShift action_32
action_58 _ = happyReduce_15

action_59 _ = happyReduce_14

action_60 _ = happyReduce_28

action_61 _ = happyReduce_8

action_62 (43) = happyShift action_95
action_62 _ = happyReduce_10

action_63 _ = happyReduce_47

action_64 (40) = happyShift action_94
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (40) = happyShift action_93
action_65 (43) = happyShift action_55
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (35) = happyShift action_71
action_66 (36) = happyShift action_72
action_66 (37) = happyShift action_73
action_66 (38) = happyShift action_74
action_66 (40) = happyShift action_92
action_66 (44) = happyShift action_75
action_66 (45) = happyShift action_76
action_66 (46) = happyShift action_77
action_66 (47) = happyShift action_78
action_66 (48) = happyShift action_79
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (43) = happyShift action_55
action_67 _ = happyReduce_33

action_68 (34) = happyShift action_91
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (29) = happyShift action_90
action_69 (35) = happyShift action_71
action_69 (36) = happyShift action_72
action_69 (37) = happyShift action_73
action_69 (38) = happyShift action_74
action_69 (44) = happyShift action_75
action_69 (45) = happyShift action_76
action_69 (46) = happyShift action_77
action_69 (47) = happyShift action_78
action_69 (48) = happyShift action_79
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (26) = happyShift action_89
action_70 (35) = happyShift action_71
action_70 (36) = happyShift action_72
action_70 (37) = happyShift action_73
action_70 (38) = happyShift action_74
action_70 (44) = happyShift action_75
action_70 (45) = happyShift action_76
action_70 (46) = happyShift action_77
action_70 (47) = happyShift action_78
action_70 (48) = happyShift action_79
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (33) = happyShift action_39
action_71 (39) = happyShift action_49
action_71 (49) = happyShift action_41
action_71 (21) = happyGoto action_88
action_71 (22) = happyGoto action_45
action_71 (23) = happyGoto action_38
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (33) = happyShift action_39
action_72 (39) = happyShift action_49
action_72 (49) = happyShift action_41
action_72 (21) = happyGoto action_87
action_72 (22) = happyGoto action_45
action_72 (23) = happyGoto action_38
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (33) = happyShift action_39
action_73 (39) = happyShift action_49
action_73 (49) = happyShift action_41
action_73 (21) = happyGoto action_86
action_73 (22) = happyGoto action_45
action_73 (23) = happyGoto action_38
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (33) = happyShift action_39
action_74 (39) = happyShift action_49
action_74 (49) = happyShift action_41
action_74 (21) = happyGoto action_85
action_74 (22) = happyGoto action_45
action_74 (23) = happyGoto action_38
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (33) = happyShift action_39
action_75 (39) = happyShift action_49
action_75 (49) = happyShift action_41
action_75 (21) = happyGoto action_84
action_75 (22) = happyGoto action_45
action_75 (23) = happyGoto action_38
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (33) = happyShift action_39
action_76 (39) = happyShift action_49
action_76 (49) = happyShift action_41
action_76 (21) = happyGoto action_83
action_76 (22) = happyGoto action_45
action_76 (23) = happyGoto action_38
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (33) = happyShift action_39
action_77 (39) = happyShift action_49
action_77 (49) = happyShift action_41
action_77 (21) = happyGoto action_82
action_77 (22) = happyGoto action_45
action_77 (23) = happyGoto action_38
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (33) = happyShift action_39
action_78 (39) = happyShift action_49
action_78 (49) = happyShift action_41
action_78 (21) = happyGoto action_81
action_78 (22) = happyGoto action_45
action_78 (23) = happyGoto action_38
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (33) = happyShift action_39
action_79 (39) = happyShift action_49
action_79 (49) = happyShift action_41
action_79 (21) = happyGoto action_80
action_79 (22) = happyGoto action_45
action_79 (23) = happyGoto action_38
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (35) = happyShift action_71
action_80 (36) = happyShift action_72
action_80 (37) = happyShift action_73
action_80 (38) = happyShift action_74
action_80 _ = happyReduce_43

action_81 (35) = happyShift action_71
action_81 (36) = happyShift action_72
action_81 (37) = happyShift action_73
action_81 (38) = happyShift action_74
action_81 _ = happyReduce_42

action_82 (35) = happyShift action_71
action_82 (36) = happyShift action_72
action_82 (37) = happyShift action_73
action_82 (38) = happyShift action_74
action_82 _ = happyReduce_41

action_83 (35) = happyShift action_71
action_83 (36) = happyShift action_72
action_83 (37) = happyShift action_73
action_83 (38) = happyShift action_74
action_83 _ = happyReduce_40

action_84 (35) = happyShift action_71
action_84 (36) = happyShift action_72
action_84 (37) = happyShift action_73
action_84 (38) = happyShift action_74
action_84 _ = happyReduce_39

action_85 _ = happyReduce_38

action_86 _ = happyReduce_37

action_87 (37) = happyShift action_73
action_87 (38) = happyShift action_74
action_87 _ = happyReduce_36

action_88 (37) = happyShift action_73
action_88 (38) = happyShift action_74
action_88 _ = happyReduce_35

action_89 (25) = happyShift action_46
action_89 (28) = happyShift action_47
action_89 (31) = happyShift action_48
action_89 (33) = happyShift action_39
action_89 (39) = happyShift action_49
action_89 (49) = happyShift action_41
action_89 (20) = happyGoto action_99
action_89 (21) = happyGoto action_44
action_89 (22) = happyGoto action_45
action_89 (23) = happyGoto action_38
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (33) = happyShift action_39
action_90 (39) = happyShift action_49
action_90 (49) = happyShift action_41
action_90 (21) = happyGoto action_98
action_90 (22) = happyGoto action_45
action_90 (23) = happyGoto action_38
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (33) = happyShift action_39
action_91 (39) = happyShift action_49
action_91 (49) = happyShift action_41
action_91 (21) = happyGoto action_97
action_91 (22) = happyGoto action_45
action_91 (23) = happyGoto action_38
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_34

action_93 _ = happyReduce_4

action_94 _ = happyReduce_3

action_95 (33) = happyShift action_39
action_95 (39) = happyShift action_40
action_95 (49) = happyShift action_41
action_95 (8) = happyGoto action_96
action_95 (22) = happyGoto action_62
action_95 (23) = happyGoto action_38
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_9

action_97 (32) = happyShift action_101
action_97 (35) = happyShift action_71
action_97 (36) = happyShift action_72
action_97 (37) = happyShift action_73
action_97 (38) = happyShift action_74
action_97 (44) = happyShift action_75
action_97 (45) = happyShift action_76
action_97 (46) = happyShift action_77
action_97 (47) = happyShift action_78
action_97 (48) = happyShift action_79
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (35) = happyShift action_71
action_98 (36) = happyShift action_72
action_98 (37) = happyShift action_73
action_98 (38) = happyShift action_74
action_98 (44) = happyShift action_75
action_98 (45) = happyShift action_76
action_98 (46) = happyShift action_77
action_98 (47) = happyShift action_78
action_98 (48) = happyShift action_79
action_98 _ = happyReduce_31

action_99 (27) = happyShift action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (25) = happyShift action_46
action_100 (28) = happyShift action_47
action_100 (31) = happyShift action_48
action_100 (33) = happyShift action_39
action_100 (39) = happyShift action_49
action_100 (49) = happyShift action_41
action_100 (20) = happyGoto action_103
action_100 (21) = happyGoto action_44
action_100 (22) = happyGoto action_45
action_100 (23) = happyGoto action_38
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (25) = happyShift action_46
action_101 (28) = happyShift action_47
action_101 (31) = happyShift action_48
action_101 (33) = happyShift action_39
action_101 (39) = happyShift action_49
action_101 (49) = happyShift action_41
action_101 (20) = happyGoto action_102
action_101 (21) = happyGoto action_44
action_101 (22) = happyGoto action_45
action_101 (23) = happyGoto action_38
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_30

action_103 _ = happyReduce_29

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1] ++ happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Tuple happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Tuple happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  6 happyReduction_7
happyReduction_7  =  HappyAbsSyn6
		 (Empty
	)

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn9
		 (Btype happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (let Xtype l = happy_var_3 in Xtype $ [happy_var_1] ++ l
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (Xtype [happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (let Ttype l = happy_var_3 in Ttype $ [happy_var_1] ++ l
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (Ttype $ [happy_var_1] ++ [happy_var_3]
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 (Ttype' happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (Xtype' happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (Ftype happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (Ptype' happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  15 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (Ftype' happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  16 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_2)
	(HappyTerminal (TokenFunctionDef happy_var_1))
	 =  HappyAbsSyn16
		 (Signature happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 17 happyReduction_23
happyReduction_23 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Equation happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_2  18 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1] ++ happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  19 happyReduction_26
happyReduction_26 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn19
		 (Valdef happy_var_1 happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 19 happyReduction_27
happyReduction_27 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Typedef happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 19 happyReduction_28
happyReduction_28 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TypedefFunc happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 6 20 happyReduction_29
happyReduction_29 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Conditional happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 6 20 happyReduction_30
happyReduction_30 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 20 happyReduction_31
happyReduction_31 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1  20 happyReduction_32
happyReduction_32 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (SExpr happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  21 happyReduction_33
happyReduction_33 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  21 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (Paren happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  21 happyReduction_35
happyReduction_35 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 Plus happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  21 happyReduction_36
happyReduction_36 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 Minus happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  21 happyReduction_37
happyReduction_37 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 Times happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  21 happyReduction_38
happyReduction_38 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 Div happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  21 happyReduction_39
happyReduction_39 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 EqualTo happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  21 happyReduction_40
happyReduction_40 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 GreaterThan happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  21 happyReduction_41
happyReduction_41 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 LessThan happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  21 happyReduction_42
happyReduction_42 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 LessThanEqual happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  21 happyReduction_43
happyReduction_43 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Infix happy_var_1 GreaterThanEqual happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  22 happyReduction_44
happyReduction_44 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn22
		 (EInt happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  22 happyReduction_45
happyReduction_45 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn22
		 (ESymbol happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  22 happyReduction_46
happyReduction_46 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  22 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (Tuple happy_var_2
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  23 happyReduction_48
happyReduction_48 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn23
		 (FunctionApp happy_var_1 happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 51 51 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenType -> cont 24;
	TokenIf -> cont 25;
	TokenThen -> cont 26;
	TokenElse -> cont 27;
	TokenWhile -> cont 28;
	TokenDo -> cont 29;
	TokenOf -> cont 30;
	TokenLet -> cont 31;
	TokenIn -> cont 32;
	TokenInt happy_dollar_dollar -> cont 33;
	TokenAssign -> cont 34;
	TokenPlus -> cont 35;
	TokenMinus -> cont 36;
	TokenTimes -> cont 37;
	TokenDiv -> cont 38;
	TokenLParen -> cont 39;
	TokenRParen -> cont 40;
	TokenPipe -> cont 41;
	TokenArrow -> cont 42;
	TokenComa -> cont 43;
	TokenEQ -> cont 44;
	TokenGT -> cont 45;
	TokenLT -> cont 46;
	TokenLTE -> cont 47;
	TokenGTE -> cont 48;
	TokenSym happy_dollar_dollar -> cont 49;
	TokenFunctionDef happy_dollar_dollar -> cont 50;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 51 tk tks = happyError' (tks, explist)
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
