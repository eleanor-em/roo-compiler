{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : RooAST
Description : Grammar of our Roo AST  

Roo AST written by $team for Programming Language Implementation 
Assignment 1b. This file specifies node representations of the various components that make
up the Roo Language. Where appropriate, it specifies the origin of nodes in the source
file according to row and column. It also includes appropriate helper functions for
extracting information from nodes. 
-}

module RooAst where

import Data.Text (Text)

-----------------------------------
-- AST Nodes 
-----------------------------------

import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Text.Parsec.Pos (initialPos)

-- | A Roo Program node consists of: 
-- 
--     1. a list of records 
--     2. a list of array type definitions 
--     3. a list of procedures   
data Program = Program [Record] [ArrayType] [Procedure]
    deriving (Show, Eq)

-- | A Record node consists of:
--     
--     1. a non-empty list of field declarations 
--     2. an identifier 
data Record = Record [FieldDecl] Ident
    deriving (Show, Eq)

-- | An Array type node consists of:
--
--     1. an integer 
--     2. a type name 
--     3. an identifier 
data ArrayType = ArrayType Int LocatedTypeName Ident
    deriving (Show, Eq)

-- | A Procedure node consists of:
--
--     1. a procedure header 
--     2. a list of local variable declarations
--     3. a list of statements 
data Procedure = Procedure SourcePos ProcHeader TypeName [VarDecl] [Statement]
    deriving (Show, Eq)

-- | A Procedure Header node consists of:
--
--     1. an Identifier
--     2. a list of parameters 
data ProcHeader = ProcHeader Ident [Parameter]
    deriving (Show, Eq)

-- | A Field Declaration node consists of:
-- 
--     1. a primitive type
--     2. an identifier 
data FieldDecl = FieldDecl PrimitiveType Ident
    deriving (Show, Eq)

-- | Primitive type can either be a Raw Boolean or a Raw Int 
data PrimitiveType = RawBoolType | RawIntType
    deriving (Show, Eq)

-- | A Variable Declaration node consists of:
-- 
--     1. a type name
--     2. a non-empty list of identifiers 
data VarDecl = VarDecl LocatedTypeName [Ident]
    deriving (Show, Eq)

-- | A Parameter Type node can either be a Type Name or a Type Val
data Parameter = TypeParam LocatedTypeName Ident | ValParam LocatedTypeName Ident
    deriving (Show, Eq)

-- | A Located Type Name node tracks both the Type Name node
-- and its source position in the source code.
data LocatedTypeName = LocatedTypeName SourcePos TypeName
    deriving (Show, Eq)

-- | A Type Name node can either by a Primitive Type or a type Alias 
data TypeName = PrimitiveTypeName PrimitiveType
              | AliasTypeName Ident
              | FunctionTypeName [Parameter] TypeName
              | VoidTypeName
    deriving (Show, Eq)

-- | A Statement node can be one of the following forms:
--
--     * an assignment
--     * a read statement
--     * a write statement
--     * a writeln statement
--     * a call statement
--     * an if statement
--     * an ifelse statement
--     * a while statement 
--     * a return statement
data Statement 
    = SAssign Lvalue LocatedExpr
    | SRead Lvalue
    | SWrite LocatedExpr
    | SWriteLn LocatedExpr
    | SCall Ident [LocatedExpr]
    | SIf LocatedExpr [Statement]
    | SIfElse LocatedExpr [Statement] [Statement]
    | SWhile LocatedExpr [Statement]
    | SReturn LocatedExpr
    | STailStatement Statement
    deriving (Show, Eq)

-- | An Expression node can be one of:
--     * lvalue
--     * literal
--     * expression <binary operator> expression
--     * <unary operator> expression 
--     * a function call
--     * a lambda expression
data Expression
    = ELvalue Lvalue
    | EConst Literal
    | EBinOp BinOp LocatedExpr LocatedExpr
    | EUnOp UnOp LocatedExpr
    | EFunc Ident [LocatedExpr]
    | ELambda [Parameter] LocatedTypeName [VarDecl] [Statement]
    deriving (Show, Eq)

-- | An Expression with a source location.
data LocatedExpr = LocatedExpr { locate :: SourcePos, fromLocated :: Expression }
    deriving (Show, Eq)

-- | A Literal node can be one of:
--     * a boolean
--     * an integer 
--     * a string 
data Literal 
    = LitBool Bool
    | LitInt Int
    | LitString Text
    deriving (Show, Eq)

-- | A Binary Operator node can be one of the following in order of lowest to highest precedence
--     1. or
--     2. and
--     3. = != < <= > >=
--     4. + - 
--     5. * /
data BinOp
    = BinOr | BinAnd | BinEq | BinNeq | BinLt | BinLte | BinGt | BinGte
    | BinPlus | BinMinus | BinTimes | BinDivide
    deriving (Show, Eq, Ord)

-- | A Unary Operator node can be Not or -
data UnOp = UnNot | UnNegate
    deriving (Show, Eq)

-- | An Lvalue node can be one of the following forms:
-- 
--     * <identifier> 
--     * <identifier>.<identifier>
--     * <identifier>[<expression>]
--     * <identifier>[<expression>].<identifier
data Lvalue
    = LId Ident
    | LMember Ident Ident
    | LArray Ident LocatedExpr
    | LArrayMember Ident LocatedExpr Ident
    deriving (Show, Eq)

-- | Identifier is a non empty sequence of chars (or Text)
data Ident = Ident SourcePos Text
    deriving Eq

instance Show Ident where
    show (Ident pos ident) = concat
        [ show ident
        , ":"
        , show $ sourceLine pos
        , ":"
        , show $ sourceColumn pos ]

-----------------------------------
-- AST Helper Functions 
-----------------------------------

-- | Lifts an expression to a located one using a bogus position (for simplifying analysis).
liftExpr :: Expression -> LocatedExpr
liftExpr = LocatedExpr (initialPos "")

-- | Returns the position of an lvalue.
locateLvalue :: Lvalue -> SourcePos
locateLvalue (LId (Ident pos _)) = pos
locateLvalue (LMember (Ident pos _) _) = pos
locateLvalue (LArray (Ident pos _) _) = pos
locateLvalue (LArrayMember (Ident pos _) _ _) = pos

-- | Get the name of an lvalue.
--   Used to determine if lvalues are modified in a while body.
nameLvalue :: Lvalue -> Text
nameLvalue (LId (Ident _ name)) = name
nameLvalue (LMember (Ident _ record) (Ident _ field)) = record <> "." <> field
-- Don't waste time dynamically checking the array index
nameLvalue (LArray (Ident _ array) _) = array <> "[]"
nameLvalue (LArrayMember (Ident _ array) _ (Ident _ field)) = array <> "[]." <> field

-- | Extract the name of an identifier.
fromIdent :: Ident -> Text
fromIdent (Ident _ name) = name
