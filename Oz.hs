{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Oz
Description : Code Generation for Oz 

Oz was written by $team for Programming Language Implementation 
Assignment 3b. This file specifies functions that are used to define 
oz instructions. 
-}

module Oz where

import Data.Text (Text)

import Common ( tshow, Register(..), StackSlot )

-----------------------------------
-- Standard Oz Instructions 
-----------------------------------

-- | Create a new stack frame with an approrpiate number of slots 
ozPushStackFrame :: Int -> [Text]
ozPushStackFrame n = ["push_stack_frame " <> tshow n]

-- | Delete the current stack frame
ozPopStackFrame :: Int -> [Text]
ozPopStackFrame n = ["pop_stack_frame " <> tshow n]

-- | Load what is stored in the stack slot into the given register 
ozLoad :: Register -> StackSlot -> [Text]
ozLoad register location
    = ["load " <> tshow register <> ", " <> tshow location]

-- | Load the address stored in the stack slot into the given register
ozLoadAddress :: Register -> StackSlot -> [Text]
ozLoadAddress register location
    = ["load_address " <> tshow register <> ", " <> tshow location]

-- | Load what is in the pointer register to the value register 
ozLoadIndirect :: Register -> Register -> [Text]
ozLoadIndirect value pointer
    = ["load_indirect " <> tshow value <> ", " <> tshow pointer]

-- | Store what is in the given register into a given stack slot 
ozStore :: StackSlot -> Register -> [Text]
ozStore location register
    = ["store " <> tshow location <> ", " <> tshow register]

-- | Store what is in the pointer into a given register 
ozStoreIndirect :: Register -> Register -> [Text]
ozStoreIndirect pointer value
    = ["store_indirect " <> tshow pointer <> ", " <> tshow value]

-- | Move the content of one register into a different register 
ozMove :: Register -> Register -> [Text]
ozMove dest src = ["move " <> tshow dest <> ", " <> tshow src]

-- | Perform a binary operation 
ozBinOp :: Text -> Register -> Register -> [Text]
ozBinOp op dest src = pure $ op <> " " <> tshow dest <> ", " <> tshow src

-- | Perform a ternary operation 
ozTernOp :: Text -> Register -> Register -> Register -> [Text]
ozTernOp op dest lhs rhs
    = [op <> " " <> tshow dest <> ", " <> tshow lhs <> ", " <> tshow rhs]

-- | Load an integer constant into the register
ozIntConst :: Register -> Int -> [Text]
ozIntConst register val
    = ["int_const " <> tshow register <> ", " <> tshow val]

-- | Load a boolean constant into the register
ozBoolConst :: Register -> Bool -> [Text]
ozBoolConst register val
    = ["int_const " <> tshow register <> ", " <> convertBool val]

-- | Print an integer
ozWriteInt :: Register -> [Text]
ozWriteInt (Register 0) = ["call_builtin print_int"]
ozWriteInt register
    = ozMove (Register 0) register <> ["call_builtin print_int" ]

-- | Print a boolean
ozWriteBool :: Register -> [Text]
ozWriteBool (Register 0)
    = ["call_builtin print_bool"]
ozWriteBool register
    = ozMove (Register 0) register <> ["call_builtin print_bool"]

-- | Print a string 
ozWriteString :: Text -> [Text]
ozWriteString val
    = [ "string_const r0, \"" <> val <> "\"", "call_builtin print_string" ]

-- | Read an integer from standard input
ozReadInt :: [Text]
ozReadInt = [ "call_builtin read_int" ]

-- | Read a boolean from standard input
ozReadBool :: [Text]
ozReadBool = [ "call_builtin read_bool" ]

-- | Branch to a given label if the register holds true
ozBranchOnTrue :: Register -> Text -> [Text]
ozBranchOnTrue register label
    = ["branch_on_true " <> tshow register <> ", " <> label]  

-- | Branch to a given label if the register holds false
ozBranchOnFalse :: Register -> Text -> [Text]
ozBranchOnFalse register label
    = ["branch_on_false " <> tshow register <> ", " <> label] 

-- | Branch to a given label 
ozBranch :: Text -> [Text]
ozBranch label = ["branch_uncond " <> label]

-- | Call the procedure whose code starts with the given label
ozCall :: Text -> [Text]
ozCall procName = ["call " <> procName]

-----------------------------------
-- Enumerating potential Oz Operations
-----------------------------------

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

-----------------------------------
-- Support for Function extensions in Oz 
-----------------------------------

-- | Return the Register used for storing Oz returns
ozReturnRegister :: Register
ozReturnRegister = Register 0

-- | Returns the 'special' register used in managing vtables
ozExtraRegisters :: Int -> Register
ozExtraRegisters offset = Register (1022 - offset)

-- | Convert boolean into Oz representation
convertBool :: Bool -> Text
convertBool False = "0"
convertBool True = "1"

-- | Move our pointer to the vtable section
ozSetVPtr :: Register -> [Text]
ozSetVPtr = ozMove (Register 1023)

-- | Read from vtable section 
ozReadVPtr :: Register -> [Text]
ozReadVPtr = flip ozMove (Register 1023)

-- | Branch to the called function 
ozCmpBranchVPtr :: Int -> Text -> [Text]
ozCmpBranchVPtr index label = mconcat
    [ ozIntConst (Register 1022) index
    , ozEq (Register 1021) (Register 1022) (Register 1023)
    , ozBranchOnTrue (Register 1021) label ]

