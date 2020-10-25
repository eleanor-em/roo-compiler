{-# LANGUAGE OverloadedStrings #-}

module Oz where

import Data.Text (Text)

import Common

ozReturnRegister :: Register
ozReturnRegister = Register 0

ozExtraRegisters :: Int -> Register
ozExtraRegisters offset = Register (1023 - offset)

convertBool :: Bool -> Text
convertBool False = "0"
convertBool True = "1"

ozPushStackFrame :: Int -> [Text]
ozPushStackFrame n = ["push_stack_frame " <> tshow n]
ozPopStackFrame :: Int -> [Text]
ozPopStackFrame n = ["pop_stack_frame " <> tshow n]

ozLoad :: Register -> StackSlot -> [Text]
ozLoad register location
    = ["load " <> tshow register <> ", " <> tshow location]

ozLoadAddress :: Register -> StackSlot -> [Text]
ozLoadAddress register location
    = ["load_address " <> tshow register <> ", " <> tshow location]

ozLoadIndirect :: Register -> Register -> [Text]
ozLoadIndirect value pointer
    = ["load_indirect " <> tshow value <> ", " <> tshow pointer]

ozStore :: StackSlot -> Register -> [Text]
ozStore location register
    = ["store " <> tshow location <> ", " <> tshow register]

ozStoreIndirect :: Register -> Register -> [Text]
ozStoreIndirect pointer value
    = ["store_indirect " <> tshow pointer <> ", " <> tshow value]

ozMove :: Register -> Register -> [Text]
ozMove dest src = ["move " <> tshow dest <> ", " <> tshow src]

ozBinOp :: Text -> Register -> Register -> [Text]
ozBinOp op dest src = pure $ op <> " " <> tshow dest <> ", " <> tshow src

ozTernOp :: Text -> Register -> Register -> Register -> [Text]
ozTernOp op dest lhs rhs
    = [op <> " " <> tshow dest <> ", " <> tshow lhs <> ", " <> tshow rhs]

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

ozPlus, ozMinus, ozTimes, ozDivide, ozSubOffset :: Register -> Register -> Register -> [Text]
ozPlus   = ozTernOp "add_int"
ozMinus  = ozTernOp "sub_int"
ozTimes  = ozTernOp "mul_int"
ozDivide = ozTernOp "div_int"
ozSubOffset = ozTernOp "sub_offset"

ozIntConst :: Register -> Int -> [Text]
ozIntConst register val
    = ["int_const " <> tshow register <> ", " <> tshow val]

ozBoolConst :: Register -> Bool -> [Text]
ozBoolConst register val
    = ["int_const " <> tshow register <> ", " <> convertBool val]

ozWriteInt :: Register -> [Text]
ozWriteInt (Register 0) = ["call_builtin print_int"]
ozWriteInt register
    = ozMove (Register 0) register <> ["call_builtin print_int" ]

ozWriteBool :: Register -> [Text]
ozWriteBool (Register 0)
    = ["call_builtin print_bool"]
ozWriteBool register
    = ozMove (Register 0) register <> ["call_builtin print_bool"]

ozWriteString :: Text -> [Text]
ozWriteString val
    = [ "string_const r0, \"" <> val <> "\"", "call_builtin print_string" ]

ozReadInt :: [Text]
ozReadInt = [ "call_builtin read_int" ]

ozReadBool :: [Text]
ozReadBool = [ "call_builtin read_bool" ]

ozBranchOnTrue :: Register -> Text -> [Text]
ozBranchOnTrue register label
    = ["branch_on_true " <> tshow register <> ", " <> label]  

ozBranchOnFalse :: Register -> Text -> [Text]
ozBranchOnFalse register label
    = ["branch_on_false " <> tshow register <> ", " <> label] 

ozBranch :: Text -> [Text]
ozBranch label = ["branch_uncond " <> label]

ozCall :: Text -> [Text]
ozCall procName = ["call " <> procName]