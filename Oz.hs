module Oz where

convertBool :: Bool -> String
convertBool False = "0"
convertBool True = "1"

ozBinOp :: String -> Int -> Int -> [String]
ozBinOp op dest src = pure $ op <> " " <> toOz dest <> ", " <> toOz src

ozTernOp :: String -> Int -> Int -> Int -> [String]
ozTernOp op dest lhs rhs = pure $ op <> " " <> toOz dest <> ", " <> toOz lhs <> ", " <> toOz rhs

toOz :: Int -> String
toOz register = 'r' : show register

ozLoad :: Int -> Int -> [String]
ozLoad register location = ["load " <> toOz register <> ", " <> show location]

ozNot, ozNeg :: Int -> Int -> [String]
ozNot = ozBinOp "not"
ozNeg = ozBinOp "neg_int"

ozOr, ozAnd, ozEq, ozNeq, ozLt, ozLte, ozGt, ozGte :: Int -> Int -> Int -> [String]
ozOr  = ozTernOp "or"
ozAnd = ozTernOp "and"
ozEq  = ozTernOp "cmp_eq_int"
ozNeq = ozTernOp "cmp_ne_int"
ozLt  = ozTernOp "cmp_lt_int"
ozLte = ozTernOp "cmp_le_int"
ozGt  = ozTernOp "cmp_gt_int"
ozGte = ozTernOp "cmp_ge_int"

ozPlus, ozMinus, ozTimes, ozDivide :: Int -> Int -> Int -> [String]
ozPlus   = ozTernOp "add_int"
ozMinus  = ozTernOp "sub_int"
ozTimes  = ozTernOp "mul_int"
ozDivide = ozTernOp "div_int"

ozIntConst :: Int -> Integer -> [String]
ozIntConst register val = ["int_const " <> toOz register <> ", " <> show val]

ozBoolConst :: Int -> Bool -> [String]
ozBoolConst register val = ["int_const " <> toOz register <> ", " <> convertBool val]

ozWriteInt :: Int -> [String]
ozWriteInt register =
    [ "load r0, \"" <> toOz register <> "\""
    , "call_builtin print_int" ]

ozWriteBool :: Int -> [String]
ozWriteBool register =
    [ "load r0, \"" <> toOz register <> "\""
    , "call_builtin print_bool" ]

ozWriteString :: String -> [String]
ozWriteString val =
    [ "string_const r0, \"" <> val <> "\""
    , "call_builtin print_string" ]
