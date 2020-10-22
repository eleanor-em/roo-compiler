{-# LANGUAGE OverloadedStrings #-}

module Oz where

import Data.Text (Text)

import Common

convertBool :: Bool -> Text
convertBool False = "0"
convertBool True = "1"

ozPushStackFrame :: Int -> [Text]
ozPushStackFrame n = ["push_stack_frame " <> tshow n]
ozPopStackFrame :: Int -> [Text]
ozPopStackFrame n = ["pop_stack_frame " <> tshow n]

ozLoad :: Register -> StackSlot -> [Text]
ozLoad register location = ["load " <> tshow register <> ", " <> tshow location]

ozLoadIndirect :: Register -> Register -> [Text]
ozLoadIndirect value pointer = ["load_indirect " <> tshow value <> ", " <> tshow pointer]

ozStore :: StackSlot -> Register -> [Text]
ozStore location register = ["store " <> tshow location <> ", " <> tshow register]

ozStoreIndirect :: Register -> Register -> [Text]
ozStoreIndirect pointer value = ["store_indirect " <> tshow pointer <> ", " <> tshow value]

ozMove :: Register -> Register -> [Text]
ozMove dest src = ["move " <> tshow dest <> ", " <> tshow src]

ozBinOp :: Text -> Register -> Register -> [Text]
ozBinOp op dest src = pure $ op <> " " <> tshow dest <> ", " <> tshow src

ozTernOp :: Text -> Register -> Register -> Register -> [Text]
ozTernOp op dest lhs rhs = pure $ op <> " " <> tshow dest <> ", " <> tshow lhs <> ", " <> tshow rhs

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

ozIntConst :: Register -> Int -> [Text]
ozIntConst register val = ["int_const " <> tshow register <> ", " <> tshow val]

ozBoolConst :: Register -> Bool -> [Text]
ozBoolConst register val = ["int_const " <> tshow register <> ", " <> convertBool val]

ozWriteInt :: Register -> [Text]
ozWriteInt (Register 0) = ["call_builtin print_int"]
ozWriteInt register = ozMove (Register 0) register <> ["call_builtin print_int" ]

ozWriteBool :: Register -> [Text]
ozWriteBool (Register 0) = ["call_builtin print_bool"]
ozWriteBool register = ozMove (Register 0) register <> ["call_builtin print_bool"]

ozWriteString :: Text -> [Text]
ozWriteString val =
    [ "string_const r0, \"" <> val <> "\""
    , "call_builtin print_string" ]

ozReadInt :: StackSlot -> [Text]
ozReadInt slot = [ "call_builtin read_int" ] <> ozStore slot (Register 0)

ozReadIntIndirect :: StackSlot -> [Text]
ozReadIntIndirect pointer = mconcat
    [ ozLoad (Register 1) pointer
    , [ "call_builtin read_int" ]
    , ozStoreIndirect (Register 1) (Register 0) ]

ozReadBool :: StackSlot -> [Text]
ozReadBool slot = [ "call_builtin read_bool" ] <> ozStore slot (Register 0)

ozReadBoolIndirect :: StackSlot -> [Text]
ozReadBoolIndirect pointer = mconcat
    [ ozLoad (Register 1) pointer
    , [ "call_builtin read_bool" ]
    , ozStoreIndirect (Register 1) (Register 0) ]

ozBranchOnTrue :: Register -> Text -> [Text]
ozBranchOnTrue register label = ["branch_on_true" <> tshow register <> label]  

ozBranchOnFalse :: Register -> Text -> [Text]
ozBranchOnFalse register label = ["branch_on_false" <> tshow register <> label] 

ozBranch :: Text -> [Text]
ozBranch label = ["branch_uncond " <> label]

ozCall :: Text -> [Text]
ozCall procName = ["call " <> procName]