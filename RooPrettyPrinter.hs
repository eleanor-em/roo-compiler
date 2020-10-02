{-|
Module      : RooPrettyPrinter
Description : Pretty printer for the Roo language 

Pretty printer written by $team for Programming Language Implementation 
Assignment 1b. This program takes in an AST as generated by the RooParser for some source code 
and returns a string representing the pretty printed version of the source code. 
-}

module RooPrettyPrinter(prettyPrint) where 

import Data.List (intercalate)
import Control.Monad.State
import RooAst

indentation = "    "
endline = ";\n"

-----------------------------------
-- Main Pretty Printer 
-----------------------------------

-- | Take the AST of a program and return a pretty print formatted string 
prettyPrint :: Program -> String
prettyPrint (Program records arrays procs) = concat
    [ concat (map prettyRecord records)
    , concat (map prettyArrayDecl arrays)
    , if length records > 0 || length arrays > 0 then "\n" else ""
    , intercalate "\n" (map prettyProcedure procs) ]

-----------------------------------
-- Main Definitions Pretty Printers 
-----------------------------------

-- | Replaces a Record node with its pretty-printed representation 
prettyRecord :: Record -> String
prettyRecord (Record recordFields ident) = concat
    [ "record\n" ++ indentation ++ "{ "
    , intercalate ("\n" ++ indentation ++ "; ") $ map prettyFieldDecl recordFields
    , "\n"
    , indentation ++ "} " ++ ident ++ endline ]

-- | Replaces an Array Declaration node with its pretty-printed representation
prettyArrayDecl :: ArrayType -> String
prettyArrayDecl (ArrayType size typeName ident) = concat
    [ "array[" ++ show size ++ "] "
    , prettyType typeName
    , " " ++ ident ++ endline ]

-- | Replaces a Procedure node with its pretty-printed representation
prettyProcedure :: Procedure -> String
prettyProcedure (Procedure header varDecls body) = concat
    [ prettyHeader header
    , concat (map prettyVarDecls varDecls)
    , "{\n"
    , prettyAllStatements body 1
    , "}\n" ]

-----------------------------------
-- Type Definitions & Declarations Pretty Printers
-----------------------------------

-- | Replaces a Primitive data type node with a String representation of primitive types
prettyPrimitiveType :: PrimitiveType -> String
prettyPrimitiveType RawBoolType = "boolean"
prettyPrimitiveType RawIntType = "integer"

-- | Replaces a TypeName node with a String representation of primitive types
prettyType :: TypeName -> String
prettyType (PrimitiveTypeName primitiveType) = prettyPrimitiveType primitiveType
prettyType (AliasTypeName ident) = ident

-- | Replaces a FieldDecl node with a string 
prettyFieldDecl :: FieldDecl -> String 
prettyFieldDecl (FieldDecl fieldType fieldIdent)
    = (prettyPrimitiveType fieldType) ++ " " ++ fieldIdent

-----------------------------------
-- Procedure Helper Pretty Printers 
-----------------------------------

-- | Replaces a Procedure Header node with a string represenation of the Procedure Header content
prettyHeader :: ProcHeader -> String 
prettyHeader (ProcHeader procName headerParams) = concat
    [ "procedure " ++ procName  ++ " "
    , prettyParens $ intercalate ", " $ map prettyParameter headerParams
    , "\n" ]

-- | Replaces a Parameter node with a string representation of a Parameter
prettyParameter :: Parameter -> String 
prettyParameter (TypeParam typeName ident)
    = prettyType typeName ++ " " ++ ident

prettyParameter (ValParam typeName ident)
    = prettyType typeName ++ " val " ++ ident

-- | Replaces a Variable Declaration node with a string represenation of the variable declarations 
prettyVarDecls :: VarDecl -> String 
prettyVarDecls (VarDecl typeName varIdents) = concat
    [ indentation ++ prettyType typeName ++ " "
    , intercalate ", " varIdents
    , endline ]

-- | Replaces a list of Statement nodes with its pretty-printed representation, with all statements appropriately formatted
prettyAllStatements :: [Statement] -> Int -> String
prettyAllStatements body startingIndent
    = concat $ map (prettyStatement startingIndent) body

-----------------------------------
-- Statements Pretty Printers 
-----------------------------------

-- | Replaces a statement node with a its pretty-printed representation.
-- The `Int` argument represents the current indentation level.
prettyStatement :: Int -> Statement -> String
prettyStatement indentLevel statement
    = indents ++ case statement of
        SAssign lvalue expr -> prettyLvalue lvalue ++ " <- " ++ prettyExpr expr ++ endline
        SRead lvalue -> "read " ++ prettyLvalue lvalue ++ endline
        SWrite expr -> "write " ++ prettyExpr expr ++ endline
        SWriteLn expr -> "writeln " ++ prettyExpr expr ++ endline
        SCall ident arglist -> concat
            [ "call " ++ ident
            , prettyParens (intercalate ", " $ map prettyExpr arglist)
            , endline ]
        SIf expr body -> concat
            [ "if " ++ prettyExpr expr ++ " then\n"
            , prettyAllStatements body (indentLevel + 1)
            , indents ++ "fi\n"]
        SIfElse expr ifBody elseBody -> concat
            [ "if " ++ prettyExpr expr ++ " then\n"
            , prettyAllStatements ifBody (indentLevel + 1)
            , indents ++ "else\n"
            , prettyAllStatements elseBody (indentLevel + 1)
            , indents ++ "fi\n" ]
        SWhile expr body -> concat
                [ "while " ++ prettyExpr expr ++ " do\n"
                , prettyAllStatements body (indentLevel + 1 )
                , indents ++ "od\n" ]
    where
        -- Shorthand for all of the indents we currently need
        indents = allIndents indentLevel

-- | Generates a string containing `n` indentations.
allIndents :: Int -> String
allIndents n = concat $ take n $ cycle [indentation]

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
prettyExpr :: Expression -> String 
prettyExpr (ELvalue lvalue) = prettyLvalue lvalue 
prettyExpr (EConst literal) = prettyLiteral literal
prettyExpr expr = prettyGetExpr expr OpNone

-- | Replace an Expression node with a string represenation of the expression, 
-- while keeping track of whether there was a parent operator to this expression
prettyGetExpr :: Expression -> ParentOp -> String
prettyGetExpr (ELvalue lvalue) _ = prettyLvalue lvalue 
prettyGetExpr (EConst literal) _ = prettyLiteral literal

-- | Basic binary operator case with no parent operator
prettyGetExpr (EBinOp op lhs rhs) OpNone = intercalate " "
    [ prettyGetExpr lhs (OpRight op)
    , prettyBinOp op
    , prettyGetExpr rhs (OpLeft op) ]

-- | Special case for binary operators: / is not associative with * on the left side, so we need to parenthesise the expression
prettyGetExpr expr@(EBinOp BinDivide lhs rhs) (OpLeft BinTimes)
    = prettyParensExpr expr

-- | Handles the case where the parent operator is on the left. If the operators don't associate, and
-- the parent doesn't have lower precedence, we need to parenthesise.
prettyGetExpr expr@(EBinOp op lhs rhs) (OpLeft parentOp)
    = parenthesiseIf expr $ not (isLeftAssociative op parentOp) && parentOp >= op || (nonAssociative op parentOp)

-- | Handle the case where the parent operator is on the right. If the operators don't associate, and 
-- the parent operator has higher precedence, we need to parenthesise.
prettyGetExpr expr@(EBinOp op lhs rhs) (OpRight parentOp)
    = parenthesiseIf expr $ parentOp `binaryHigherPrecedence` op || (nonAssociative op parentOp)

-- | Dealing with unary operator precedence 
prettyGetExpr (EUnOp op expr@(EBinOp innerOp lhs rhs)) OpNone
    = prettyUnOp op ++ (parenthesiseIf expr $ unaryHigherPrecedence op innerOp)

prettyGetExpr expr@(EUnOp op _) (OpRight parentOp)
    = parenthesiseIf expr $ not $ unaryHigherPrecedence op parentOp

-- | Handling the base unary operator case 
prettyGetExpr (EUnOp op expr) _
    = prettyUnOp op ++ prettyExpr expr

-----------------------------------
-- Expression Helper Pretty Printers 
-----------------------------------

-- | Pretty-prints and parenthesises the expression
prettyParensExpr :: Expression -> String
prettyParensExpr expr = prettyParens $ prettyGetExpr expr OpNone

-- | Given an expression and an associated condition, generate
-- a parenthesised pretty-printed representation if the condition has been satisfied, 
-- otherwise return just the pretty-printed representation of the expression
parenthesiseIf :: Expression -> Bool -> String
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
nonAssociativeBinOp = [BinEq, BinNeq, BinLt, BinLte, BinGt, BinGte]

-- | Return True if the binary operators are one of the non associative relational operators 
nonAssociative :: BinOp -> BinOp -> Bool 
nonAssociative lOp rOp = (lOp `elem` nonAssociativeBinOp) && (rOp `elem` nonAssociativeBinOp)

-- | Return True if and only if the left operator has higher precedence than the right operator.
-- Uses the Ord instance, with a helper function to determine operators that are logically
-- on the same level.
binaryHigherPrecedence :: BinOp -> BinOp -> Bool
binaryHigherPrecedence lOp rOp = not (samePrecedence lOp rOp) && lOp > rOp

-- | Helper function for the above. True if and only if the binary operators have the same precedence. 
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
prettySquares :: String -> String
prettySquares str = '[' : (str ++ "]")

-- | Replaces an `LValue` node with a string representation of the given lvalue, proceeding by cases
-- in the natural manner
prettyLvalue :: LValue -> String
prettyLvalue (LId ident) = ident

prettyLvalue (LMember memberRecord memberField) 
    = memberRecord ++ "." ++ memberField

prettyLvalue (LArray arrayIdent arrayIndex)
    = arrayIdent ++ prettySquares (prettyExpr arrayIndex)

prettyLvalue (LArrayMember arrayIdent arrayMemberIndex arrayMemberField) = concat
    [ arrayIdent
    , prettySquares (prettyExpr arrayMemberIndex)
    , "."
    , arrayMemberField ]

-- | Replace a given string with a chosen string  
replace :: String -> String -> String -> String
replace "" _  _ = ""
replace _  "" _ = ""
replace haystack old new
    = if length haystack >= length old then
        if take (length old) haystack == old then
            new ++ replace (drop (length old) haystack) old new
        else
            head haystack : replace (tail haystack) old new
    else
        haystack

-- | Replaces a `Literal` node with a pretty-printed representation of the given literal 
prettyLiteral :: Literal -> String
prettyLiteral (LitBool True) = "true"
prettyLiteral (LitBool False) = "false"
prettyLiteral (LitInt num) = show num
-- the below case is a bit weird, can't use Show because that will mess up unicode characters
prettyLiteral (LitString rawString) = '"' : (rawString ++ "\"")
    -- where 
    --     escapedQuotes = replace rawString     "\"" "\\\""
    --     escapedTabs   = replace escapedQuotes "\t" "\\t"
    --     escapedString = replace escapedTabs   "\n" "\\n"

-- | Replaces a binary operator with a pretty-printed representation of the given binary operator
prettyBinOp :: BinOp -> String
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
prettyUnOp :: UnOp -> String
prettyUnOp UnNot = "not "
prettyUnOp UnNegate = "-"

-----------------------------------
-- General Helper Pretty Printers 
-----------------------------------

-- | Wrap a string with parentheses 
prettyParens :: String -> String
prettyParens str = '(' : (str ++ ")")