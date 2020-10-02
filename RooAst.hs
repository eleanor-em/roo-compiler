{-|
Module      : RooAST
Description : Grammar of our Roo AST  

Roo AST written by $team for Programming Language Implementation 
Assignment 1b. This file specifies node representations of the various components that make
up the Roo Language.  
-}

module RooAst where

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
data Record = Record
    { recordFields :: [FieldDecl]
    , recordIdent  :: Ident }
    deriving (Show, Eq)

-- | An Array type node consists of:
--
--     1. an integer 
--     2. a type name 
--     3. an identifier 
data ArrayType = ArrayType Integer TypeName Ident
    deriving (Show, Eq)

-- | A Procedure node consists of:
--
--     1. a procedure header 
--     2. a list of local variable declarations
--     3. a list of statements 
data Procedure = Procedure ProcHeader [VarDecl] [Statement]
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

-- | Primitive type can either be a Raw Boolean or a Raw Integer 
data PrimitiveType = RawBoolType | RawIntType
    deriving (Show, Eq)

-- | A Variable Declaration node consists of:
-- 
--     1. a type name
--     2. a non-empty list of identifiers 
data VarDecl = VarDecl TypeName [Ident]
    deriving (Show, Eq)

-- | A Parameter Type node can either be a Type Name or a Type Val
data Parameter = TypeParam TypeName Ident | ValParam TypeName Ident
    deriving (Show, Eq)

-- | A Type Name node can either by a Primitive Type or a type Alias 
data TypeName = PrimitiveTypeName PrimitiveType | AliasTypeName Ident
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
data Statement 
    = SAssign LValue Expression
    | SRead LValue
    | SWrite Expression
    | SWriteLn Expression
    | SCall Ident [Expression]
    | SIf Expression [Statement]
    | SIfElse Expression [Statement] [Statement]
    | SWhile Expression [Statement]
    deriving (Show, Eq)

-- | An Expression node can be one of:
--     * lvalue
--     * literal
--     * expression <binary operator> expression
--     * <unary operator> expression 
data Expression
    = ELvalue LValue
    | EConst Literal
    | EBinOp BinOp Expression Expression
    | EUnOp UnOp Expression
    deriving (Show, Eq) 

-- | A Literal node can be one of:
--     * a boolean
--     * an integer 
--     * a string 
data Literal 
    = LitBool Bool
    | LitInt Integer
    | LitString String
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

-- | An LValue node can be one of the following forms:
-- 
--     * <identifier> 
--     * <identifier>.<identifier>
--     * <identifier>[<expression>]
--     * <identifier>[<expression>].<identifier
data LValue
    = LId Ident
    | LMember Ident Ident
    | LArray Ident Expression
    | LArrayMember Ident Expression Ident
    deriving (Show, Eq)

-- | Identifier is a non empty sequence of chars (or String)
type Ident = String
