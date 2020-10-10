{-# LANGUAGE OverloadedStrings #-}

module Oz where

import Data.Text (Text)

import Common

convertBool :: Bool -> Text
convertBool False = "0"
convertBool True = "1"

toOz :: Register -> Text
toOz register = "r" <> tshow register

ozPushStackFrame :: Int -> [Text]
ozPushStackFrame n = ["push_stack_frame " <> tshow n]
ozPopStackFrame :: Int -> [Text]
ozPopStackFrame n = ["pop_stack_frame " <> tshow n]

ozLoad :: Register -> Int -> [Text]
ozLoad register location = ["load " <> toOz register <> ", " <> tshow location]

ozStore :: Int -> Register -> [Text]
ozStore location register = ["store " <> tshow location <> ", " <> toOz register]

ozBinOp :: Text -> Register -> Register -> [Text]
ozBinOp op dest src = pure $ op <> " " <> toOz dest <> ", " <> toOz src

ozTernOp :: Text -> Register -> Register -> Register -> [Text]
ozTernOp op dest lhs rhs = pure $ op <> " " <> toOz dest <> ", " <> toOz lhs <> ", " <> toOz rhs

ozNot, ozNeg :: Register -> Register -> [Text]
ozNot = ozBinOp "not"
ozNeg = ozBinOp "neg_int"

ozOr, ozAnd, ozEq, ozNeq, ozLt, ozLte, ozGt, ozGte :: Register -> Register -> Register -> [Text]
ozOr  = ozTernOp "or"
ozAnd = ozTernOp "and"
ozEq  = ozTernOp "cmp_eq_int"
ozNeq = ozTernOp "cmp_ne_int"
ozLt  = ozTernOp "cmp_lt_int"
ozLte = ozTernOp "cmp_le_int"
ozGt  = ozTernOp "cmp_gt_int"
ozGte = ozTernOp "cmp_ge_int"

ozPlus, ozMinus, ozTimes, ozDivide :: Register -> Register -> Register -> [Text]
ozPlus   = ozTernOp "add_int"
ozMinus  = ozTernOp "sub_int"
ozTimes  = ozTernOp "mul_int"
ozDivide = ozTernOp "div_int"

ozIntConst :: Register -> Integer -> [Text]
ozIntConst register val = ["int_const " <> toOz register <> ", " <> tshow val]

ozBoolConst :: Register -> Bool -> [Text]
ozBoolConst register val = ["int_const " <> toOz register <> ", " <> convertBool val]

ozWriteInt :: Register -> [Text]
ozWriteInt (Register 0) = ["call_builtin print_int"]
ozWriteInt register =
    [ "move r0, \"" <> toOz register <> "\""
    , "call_builtin print_int" ]

ozWriteBool :: Register -> [Text]
ozWriteBool (Register 0) = ["call_builtin print_bool"]
ozWriteBool register =
    [ "move r0, \"" <> toOz register <> "\""
    , "call_builtin print_bool" ]

ozWriteString :: Text -> [Text]
ozWriteString val =
    [ "string_const r0, \"" <> val <> "\""
    , "call_builtin print_string" ]
