{-|
Module      : RooPrettyPrinter
Description : Pretty printer for the Roo language 

Pretty printer written by $team for Programming Language Implementation 
Assignment 1b. This program takes in an AST as generated by the RooParser for some source code 
and returns a string representing the pretty printed version of the source code. 
-}

{-# LANGUAGE OverloadedStrings #-}

module RooPrettyPrinter (prettyPrint, prettyBinOp, prettyStatement, prettyExpr) where 

import Common

import Data.Text (intercalate, Text)
import qualified Data.Text as T

import RooAst

indentation :: Text
indentation = "    "

endline :: Text
endline = ";\n"

-----------------------------------
-- Main Pretty Printer 
-----------------------------------

-- | Take the AST of a program and return a pretty print formatted string 
prettyPrint :: Program -> Text
prettyPrint (Program records arrays procs) = mconcat
    [ foldMap prettyRecord records
    , foldMap prettyArrayDecl arrays
    , if not (null records && null arrays) then "\n" else ""
    , T.intercalate "\n" (map prettyProcedure procs) ]

-----------------------------------
-- Main Definitions Pretty Printers 
-----------------------------------

-- | Replaces a Record node with its pretty-printed representation 
prettyRecord :: Record -> Text
prettyRecord (Record fields ident) = mconcat
    [ "record\n" <> indentation <> "{ "
    , T.intercalate ("\n" <> indentation <> "; ") $ map prettyFieldDecl fields
    , "\n"
    , indentation <> "} " <> (fromIdent ident) <> endline ]

-- | Replaces an Array Declaration node with its pretty-printed representation
prettyArrayDecl :: ArrayType -> Text
prettyArrayDecl (ArrayType size typeName ident) = mconcat
    [ "array[" <> tshow size <> "] "
    , prettyType typeName
    , " " <> fromIdent ident <> endline ]

-- | Replaces a Procedure node with its pretty-printed representation
prettyProcedure :: Procedure -> Text
prettyProcedure (Procedure _ header VoidTypeName varDecls body) = mconcat
    [ prettyHeader header
    , "\n"
    , mconcat $ map prettyVarDecls varDecls
    , "{\n"
    , prettyAllStatements body 1
    , "}\n" ]

prettyProcedure (Procedure _ header retType varDecls body) = mconcat
    [ prettyHeader header
    , " -> "
    , prettyTypeInner retType
    , "\n"
    , mconcat $ map prettyVarDecls varDecls
    , "{\n"
    , prettyAllStatements body 1
    , "}\n" ]
-----------------------------------
-- Type Definitions & Declarations Pretty Printers
-----------------------------------

-- | Replaces a Primitive data type node with a Text representation of primitive types
prettyPrimitiveType :: PrimitiveType -> Text
prettyPrimitiveType RawBoolType = "boolean"
prettyPrimitiveType RawIntType = "integer"

-- | Replaces a TypeName node with a Text representation of primitive types
prettyType :: LocatedTypeName -> Text
prettyType (LocatedTypeName _ ty) = prettyTypeInner ty

prettyTypeInner :: TypeName -> Text
prettyTypeInner (PrimitiveTypeName primitiveType) = prettyPrimitiveType primitiveType
prettyTypeInner (AliasTypeName ident) = fromIdent ident
prettyTypeInner (FunctionTypeName params VoidTypeName)
    = "procedure(" <> foldMap prettyParameter params <> ")"
prettyTypeInner (FunctionTypeName params retType)
    = "procedure(" <> foldMap prettyParameter params <> ") -> " <> prettyTypeInner retType
prettyTypeInner VoidTypeName = "void"

-- | Replaces a FieldDecl node with a string 
prettyFieldDecl :: FieldDecl -> Text 
prettyFieldDecl (FieldDecl fieldType ident)
    = prettyPrimitiveType fieldType <> " " <> fromIdent ident

-----------------------------------
-- Procedure Helper Pretty Printers 
-----------------------------------

-- | Replaces a Procedure Header node with a string represenation of the Procedure Header content
prettyHeader :: ProcHeader -> Text 
prettyHeader (ProcHeader ident headerParams) = mconcat
    [ "procedure " <> fromIdent ident  <> " "
    , prettyParens $ T.intercalate ", " $ map prettyParameter headerParams ]

-- | Replaces a Parameter node with a string representation of a Parameter
prettyParameter :: Parameter -> Text 
prettyParameter (TypeParam typeName ident)
    = prettyType typeName <> " " <> fromIdent ident

prettyParameter (ValParam typeName ident)
    = prettyType typeName <> " val " <> fromIdent ident

-- | Replaces a Variable Declaration node with a string represenation of the variable declarations 
prettyVarDecls :: VarDecl -> Text 
prettyVarDecls (VarDecl typeName idents) = mconcat
    [ indentation <> prettyType typeName <> " "
    , T.intercalate ", " $ map fromIdent idents
    , endline ]

-- | Replaces a list of Statement nodes with its pretty-printed representation, with all statements
--   appropriately formatted
prettyAllStatements :: [Statement] -> Int -> Text
prettyAllStatements body startingIndent
    = foldMap (prettyStatement startingIndent) body

-----------------------------------
-- Statements Pretty Printers 
-----------------------------------

-- | Replaces a statement node with a its pretty-printed representation.
--   The `Int` argument represents the current indentation level.
prettyStatement :: Int -> Statement -> Text
prettyStatement indentLevel statement
    = indents <> case statement of
        SAssign lvalue expr -> prettyLvalue lvalue <> " <- " <> prettyExpr (fromLocated expr)
            <> endline
        SRead lvalue -> "read " <> prettyLvalue lvalue <> endline
        SWrite expr -> "write " <> prettyExpr (fromLocated expr) <> endline
        SWriteLn expr -> "writeln " <> prettyExpr (fromLocated expr) <> endline
        SCall ident arglist -> mconcat
            [ "call " <> (fromIdent ident)
            , prettyParens (T.intercalate ", " $ map (prettyExpr . fromLocated) arglist)
            , endline ]
        SIf expr body -> mconcat
            [ "if " <> prettyExpr (fromLocated expr) <> " then\n"
            , prettyAllStatements body (indentLevel + 1)
            , indents <> "fi\n"]
        SIfElse expr ifBody elseBody -> mconcat
            [ "if " <> prettyExpr (fromLocated expr) <> " then\n"
            , prettyAllStatements ifBody (indentLevel + 1)
            , indents <> "else\n"
            , prettyAllStatements elseBody (indentLevel + 1)
            , indents <> "fi\n" ]
        SWhile expr body -> mconcat
                [ "while " <> prettyExpr (fromLocated expr) <> " do\n"
                , prettyAllStatements body (indentLevel + 1 )
                , indents <> "od\n" ]
        SReturn expr -> "return " <> prettyExpr (fromLocated expr) <> endline
    where
        -- Shorthand for all of the indents we currently need
        indents = allIndents indentLevel

-- | Generates a string containing `n` indentations.
allIndents :: Int -> Text
allIndents n = mconcat $ replicate n indentation

-----------------------------------
-- Expression Pretty Printers 
-----------------------------------

-- | Tracks parent operators: whether they exist, and which side they're on.
data ParentOp = OpLeft BinOp | OpRight BinOp | OpNone
    deriving (Show, Eq)

-- | Replaces an expression node with a string representation of the expression
-- Base case: we ignore very external parentheses and deal with simple cases
-- Recursive case: we traverse the nested expression nodes and add parentheses where needed. 
-- The order of precedence of expressions is handled by the RooParser, so we just need to check
-- if parent (or outer) operators are to the left, right or don't exist. We handle each case 
-- as appropriate while taking into consideration special cases and associativity for different
-- operators. 
prettyExpr :: Expression -> Text 
prettyExpr (ELvalue lvalue) = prettyLvalue lvalue 
prettyExpr (EConst literal) = prettyLiteral literal
prettyExpr expr = prettyGetExpr expr OpNone

-- | Replace an Expression node with a string represenation of the expression, 
-- while keeping track of whether there was a parent operator to this expression
prettyGetExpr :: Expression -> ParentOp -> Text
prettyGetExpr (ELvalue lvalue) _ = prettyLvalue lvalue 
prettyGetExpr (EConst literal) _ = prettyLiteral literal
prettyGetExpr (EFunc (Ident _ name) args) _ = name
    <> prettyParens(intercalate ", " (map (prettyExpr . fromLocated) args))

-- | Basic binary operator case with no parent operator
prettyGetExpr (EBinOp op lhs rhs) OpNone = T.unwords
    [ prettyGetExpr (fromLocated lhs) (OpRight op)
    , prettyBinOp op
    , prettyGetExpr (fromLocated rhs) (OpLeft op) ]

-- | Special case for binary operators: / is not associative with * on the left side, so we need to
--   parenthesise the expression.
prettyGetExpr expr@(EBinOp BinDivide _ _) (OpLeft BinTimes)
    = prettyParensExpr expr

-- | Handles the case where the parent operator is on the left. If the operators don't associate,
--   the parent doesn't have lower precedence, we need to parenthesise.
prettyGetExpr expr@(EBinOp op _ _) (OpLeft parentOp) = parenthesiseIf expr $
    not (isLeftAssociative op parentOp) && parentOp >= op || (nonAssociative op parentOp)

-- | Handle the case where the parent operator is on the right. If the operators don't associate,
--   and the parent operator has higher precedence, we need to parenthesise.
prettyGetExpr expr@(EBinOp op _ _) (OpRight parentOp) = parenthesiseIf expr $
    parentOp `binaryHigherPrecedence` op || (nonAssociative op parentOp)

-- | Dealing with unary operator precedence 
prettyGetExpr (EUnOp op (LocatedExpr _ expr@(EBinOp innerOp _ _))) OpNone
    = prettyUnOp op <> (parenthesiseIf expr $ unaryHigherPrecedence op innerOp)

prettyGetExpr expr@(EUnOp op _) (OpRight parentOp)
    = parenthesiseIf expr $ not $ unaryHigherPrecedence op parentOp

-- | Handling the base unary operator case 
prettyGetExpr (EUnOp op (LocatedExpr _ expr)) _
    = prettyUnOp op <> prettyExpr expr

-- TODO: make nicer
prettyGetExpr (ELambda _ _ _ _) _ = "<lambda expression>"

-----------------------------------
-- Expression Helper Pretty Printers 
-----------------------------------

-- | Pretty-prints and parenthesises the expression
prettyParensExpr :: Expression -> Text
prettyParensExpr expr = prettyParens $ prettyGetExpr expr OpNone

-- | Given an expression and an associated condition, generate
-- a parenthesised pretty-printed representation if the condition has been satisfied, 
-- otherwise return just the pretty-printed representation of the expression
parenthesiseIf :: Expression -> Bool -> Text
parenthesiseIf expr cond
    = if cond then
        prettyParensExpr expr
    else
        prettyExpr expr

-- | Return True if the binary operators associate with each other when the left is a parent.
isLeftAssociative :: BinOp -> BinOp -> Bool 
isLeftAssociative BinTimes BinTimes = True  
isLeftAssociative BinPlus  BinPlus = True
isLeftAssociative _ _ = False

-- | Identifying the non associative relational operators 
nonAssociativeBinOp :: [BinOp]
nonAssociativeBinOp = [BinEq, BinNeq, BinLt, BinLte, BinGt, BinGte]

-- | Return True if the binary operators are one of the non associative relational operators 
nonAssociative :: BinOp -> BinOp -> Bool 
nonAssociative lOp rOp = (lOp `elem` nonAssociativeBinOp) && (rOp `elem` nonAssociativeBinOp)

-- | Return True if and only if the left operator has higher precedence than the right operator.
-- Uses the Ord instance, with a helper function to determine operators that are logically
-- on the same level.
binaryHigherPrecedence :: BinOp -> BinOp -> Bool
binaryHigherPrecedence lOp rOp = not (samePrecedence lOp rOp) && lOp > rOp

-- | Helper function for the above. True iff the binary operators have the same precedence. 
samePrecedence :: BinOp -> BinOp -> Bool 
samePrecedence BinDivide BinTimes  = True 
samePrecedence BinTimes  BinDivide = True 
samePrecedence BinPlus   BinMinus  = True 
samePrecedence BinMinus  BinPlus   = True 
samePrecedence lOp       rOp       = lOp == rOp

-- | Returns True if and only if the unary operator has higher precedence than the binary operator.
unaryHigherPrecedence :: UnOp -> BinOp -> Bool
unaryHigherPrecedence UnNot binOp = case binOp of
    BinOr  -> True
    BinAnd -> True
    _      -> False

unaryHigherPrecedence UnNegate _ = True

-- | Wrap a string with square brackets 
prettySquares :: Text -> Text
prettySquares str = "[" <> str <> "]"

-- | Replaces an `Lvalue` node with a string representation of the given lvalue, proceeding by cases
-- in the natural manner
prettyLvalue :: Lvalue -> Text
prettyLvalue (LId ident) = (fromIdent ident)

prettyLvalue (LMember memberRecord memberField) 
    = (fromIdent memberRecord) <> "." <> (fromIdent memberField)

prettyLvalue (LArray ident arrayIndex)
    = (fromIdent ident) <> prettySquares (prettyExpr (fromLocated arrayIndex))

prettyLvalue (LArrayMember arrayIdent arrayMemberIndex arrayMemberField) = mconcat
    [ (fromIdent arrayIdent)
    , prettySquares (prettyExpr (fromLocated arrayMemberIndex))
    , "."
    , (fromIdent arrayMemberField) ]

-- | Replaces a `Literal` node with a pretty-printed representation of the given literal 
prettyLiteral :: Literal -> Text
prettyLiteral (LitBool True) = "true"
prettyLiteral (LitBool False) = "false"
prettyLiteral (LitInt num) = tshow num
-- the below case is a bit weird, can't use Show because that will mess up unicode characters
prettyLiteral (LitString rawString) = "\"" <> rawString <> "\""
    -- where 
    --     escapedQuotes = replace rawString     "\"" "\\\""
    --     escapedTabs   = replace escapedQuotes "\t" "\\t"
    --     escapedString = replace escapedTabs   "\n" "\\n"

-- | Replaces a binary operator with a pretty-printed representation of the given binary operator
prettyBinOp :: BinOp -> Text
prettyBinOp BinOr = "or"
prettyBinOp BinAnd = "and"
prettyBinOp BinEq = "="
prettyBinOp BinNeq = "!="
prettyBinOp BinLt = "<"
prettyBinOp BinLte = "<="
prettyBinOp BinGt = ">"
prettyBinOp BinGte = ">="
prettyBinOp BinPlus = "+"
prettyBinOp BinMinus = "-"
prettyBinOp BinTimes = "*"
prettyBinOp BinDivide = "/"

-- | Replaces a unary operator with a string representation of the given unary operator 
prettyUnOp :: UnOp -> Text
prettyUnOp UnNot = "not "
prettyUnOp UnNegate = "-"

-----------------------------------
-- General Helper Pretty Printers 
-----------------------------------

-- | Wrap a string with parentheses 
prettyParens :: Text -> Text
prettyParens str = "(" <> str <> ")"
