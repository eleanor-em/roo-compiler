{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|
Module      : RooParser
Description : Parses a Roo Program and returns a Roo AST   

Roo Parser written by $team for Programming Language Implementation 
Assignment 1b. This program parses a string to determine whether it appropriately matches the
grammar of the Language Roo and returns an Abstract Syntax Tree that represents the structure 
of the source code. 
-}

module RooParser where

import Control.Applicative (liftA2)

import qualified Data.Text as T

import RooAst

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Q

import Data.Functor (($>))

type Parser a = Parsec String Int a

type ParsedAst = Program

-----------------------------------
-- Utility Functions
-----------------------------------
sourcePos :: Parser SourcePos
sourcePos = statePos <$> getParserState

liftSourcePos :: Parser Expression -> Parser LocatedExpr
liftSourcePos = liftA2 LocatedExpr sourcePos

-----------------------------------
-- Scanner Definitions
-----------------------------------

scanner :: Q.TokenParser Int
scanner = Q.makeTokenParser
    (emptyDef
    { Q.commentLine     = "#"
    , Q.nestedComments  = True
    , Q.identStart      = letter
    , Q.identLetter     = alphaNum <|> oneOf "_'"
    , Q.opStart         = oneOf "=!<>+-*/."
    , Q.opLetter        = oneOf "="
    --  NOTE: we avoid the use of constants for each of these reserved keywords as they are only 
    --  used a couple times and the inclusion of constants would overcomplicate constant naming
    --  due to naming conflicts between files and modules. It's clearer (IMO) this way.
    , Q.reservedNames   = [ "read", "write", "writeln", "call", "if", "then"
                          , "else", "fi", "procedure", "array", "record", "while"
                          , "do", "od", "integer", "boolean", "val", "true", "false" ]
    , Q.reservedOpNames = [ "or", "and", "not", "=", "!=", "<", "<=", ">"
                          , ">=", "+", "-", "*", "/", "<-", ".", "->" ]
    , Q.caseSensitive   = True
    })

whiteSpace :: Parser ()
whiteSpace = Q.whiteSpace scanner

lexeme, parens, braces, brackets :: Parser a -> Parser a
lexeme     = Q.lexeme scanner
parens     = Q.parens scanner
braces     = Q.braces scanner
brackets   = Q.brackets scanner
decimal :: Parser Int
decimal    = lexeme $ fromIntegral <$> Q.decimal scanner

identifier, semi, comma, dot :: Parser String
identifier = lexeme $ Q.identifier scanner
semi       = lexeme $ Q.semi scanner
comma      = lexeme $ Q.comma scanner
dot        = lexeme $ Q.dot scanner

quote :: Parser Char
quote      = char '"'

reserved, reservedOp :: String -> Parser ()
reserved   = lexeme . Q.reserved scanner
reservedOp = Q.reservedOp scanner

-----------------------------------
-- Main Program Parsing
-----------------------------------

-- | Parses a Roo program and returns a Program root node if accepted. 
pProgram :: Parser Program
pProgram = do
    whiteSpace
    records <- many pRecord
    arrays <- many pArrayType
    procedures <- many1 pFunction
    eof
    return $ Program records arrays procedures

-----------------------------------
-- Main Definition Parsing
-----------------------------------

-- | Parses a record and returns a Record node if accepted
pRecord :: Parser Record
pRecord = do
        reserved "record"
        decls <- braces (pFieldDecl `sepBy1` semi)
        ident <- pIdent
        semi
        return (Record decls ident)
    <?>
        "record"

-- | Parses an arrayType declaration and returns an ArrayType node if accepted 
pArrayType :: Parser ArrayType 
pArrayType = do
        reserved "array"
        numElems <- brackets pPositiveInt
        typeName <- pTypeName
        ident <- pIdent 
        semi
        return (ArrayType numElems typeName ident)
    <?>
        "array type"

pFunction :: Parser Procedure
pFunction = do
        pos <- sourcePos
        reserved "procedure"
        header <- pHeader
        retType <- option VoidTypeName (PrimitiveTypeName <$> (reservedOp "->" *> pPrimitiveType))
        varDecls <- many pVarDecl
        body <- braces (many1 pStatement)
        return $ Procedure pos header retType varDecls body
    <?>
        "function declaration"

-----------------------------------
-- Type Definitions & Declarations Helper Functions
-----------------------------------

-- | Parses an identifier and returns an Ident node if accepted
pIdent :: Parser Ident
pIdent = liftA2 Ident sourcePos (T.pack <$> lexeme identifier)
    <?> "identifier"

-- | Parses a field declaration and returns a FieldDecl node if accepted
pFieldDecl :: Parser FieldDecl
pFieldDecl = liftA2 FieldDecl pPrimitiveType pIdent

-- | Parses a type name and returns a TypeName node if accepted 
pTypeName :: Parser LocatedTypeName
pTypeName = liftA2 LocatedTypeName sourcePos pTypeNameInner

pTypeNameInner :: Parser TypeName
pTypeNameInner =
        AliasTypeName <$> pIdent
    <|>
        PrimitiveTypeName <$> pPrimitiveType
    <|> 
        pFunctionType

-- | Parses a primitive node and returns a PrimitiveType node if accepted
pPrimitiveType :: Parser PrimitiveType
pPrimitiveType =
        reserved "boolean" $> RawBoolType
    <|>
        reserved "integer" $> RawIntType

pFunctionType :: Parser TypeName
pFunctionType = do
    reserved "procedure"
    params <- parens (pFormalParam `sepBy` comma)
    retType <- option VoidTypeName (PrimitiveTypeName <$> (reservedOp "->" *> pPrimitiveType))
    return $ FunctionTypeName params retType

-- | Parses a positive integer and returns an Int node if accepted 
pPositiveInt :: Parser Int
pPositiveInt = do
        leading <- oneOf "123456789"
        trailing <- lexeme $ many digit
        return $ read (leading:trailing)
    <?>
        "positive integer"

-----------------------------------
-- Procedure Parsing Helper Functions
-----------------------------------

-- | Parses any statement and returns a Statement node if accepted 
pStatement :: Parser Statement
pStatement = choice
     [ pAtomicStatement, try pIfElseStatement -- `try` in case it's just a regular if statement
     , pIfStatement, pWhileStatement ]
    <?>
        "statement"

-- | Parses a procedure header and returns a ProcHeader node if accepted 
pHeader :: Parser ProcHeader
pHeader =
    let pParams = parens (pFormalParam `sepBy` comma) in
        liftA2 ProcHeader pIdent pParams
    <?> 
        "header"

-- | Parses a formal parameter and returns a Formal Parameter node if accepted 
pFormalParam :: Parser Parameter 
pFormalParam =
    try pFormalValParam <|> pFormalTypeParam

-- | Parses a formal type paramter and returns a Parameter node if accepted
pFormalTypeParam :: Parser Parameter
pFormalTypeParam = liftA2 TypeParam pTypeName pIdent

-- | Parses a formal val type paramter and returns a Parameter node if accepted   
pFormalValParam :: Parser Parameter
pFormalValParam = do
        pos <- sourcePos
        typename <- pPrimitiveType
        reserved "val"
        ValParam (LocatedTypeName pos $ PrimitiveTypeName typename) <$> pIdent
    <|> do
        -- Function types are always value (they are really references, but in a roundabout manner)
        pos <- sourcePos
        typename <- pFunctionType
        ValParam (LocatedTypeName pos typename) <$> pIdent

-- | Parses a variable declaration and returns a VarDecl node if accepted
pVarDecl :: Parser VarDecl
pVarDecl =
        liftA2 VarDecl pTypeName (pIdent `sepBy1` comma)
            <* semi
    <?>
        "variable declaration"

-----------------------------------
-- Statement Parsing Helper Functions
-----------------------------------

-- | Parses any atomic statement and returns a Statement node if accepted 
pAtomicStatement :: Parser Statement
pAtomicStatement = choice
    [ try pReturnStatement, pWriteStatement, pWriteLnStatement
    , pReadStatement, pAssignStatement, pCallStatement] <* semi

-- | Parses an assignment statement and returns a Statement node if accepted 
pAssignStatement :: Parser Statement
pAssignStatement = do
    lvalue <- pLvalue
    reservedOp "<-"
    SAssign lvalue <$> pExpression

-- | Parses lvalue statements and returns an Lvalue node if accepted 
pLvalue :: Parser Lvalue
pLvalue =
        try (do
            ident1 <- pIdent
            array <- brackets pExpression
            dot
            LArrayMember ident1 array <$> pIdent)
    <|>
        try (do
            ident1 <- pIdent 
            dot
            LMember ident1 <$> pIdent)
    <|>
        try (liftA2 LArray pIdent (brackets pExpression))
    <|>
        LId <$> pIdent

-- | Parses a write statement and returns a Statement node if accepted 
pWriteStatement :: Parser Statement
pWriteStatement =
    SWrite <$> (reserved "write" *> pExpression)

-- | Parses a writeln statement and returns a Statement node if accepted
pWriteLnStatement :: Parser Statement
pWriteLnStatement =
    SWriteLn <$> (reserved "writeln" *> pExpression)

-- | Parses a call statement and returns a Statement node if accepted 
pCallStatement :: Parser Statement
pCallStatement =
    reserved "call" *> liftA2 SCall pIdent (parens (pExpression `sepBy` comma))

-- | Parses a read statement and returns a Statement node if accepted
pReadStatement :: Parser Statement
pReadStatement =
    SRead <$> (reserved "read" *> pLvalue)

-- | Hacky way to parse "return" without making it a reserved keyword.
pReturnKeyword :: Parser ()
pReturnKeyword = do
    lexeme $ char 'r'
    char 'e'
    char 't'
    char 'u'
    char 'r'
    lexeme $ char 'n'
    return ()

pReturnStatement :: Parser Statement
pReturnStatement = SReturn <$> (pReturnKeyword *> pExpression)

-- | Parses an if statement and returns a Statement node if accepted 
pIfStatement :: Parser Statement 
pIfStatement = do
    reserved "if"
    condition <- pExpression
    reserved "then"
    body <- many1 pStatement <?> "statement, \"else\" or \"fi\""
    reserved "fi"
    return $ SIf condition body

-- | Parses an ifelse statement and returns a Statement node if accepted 
pIfElseStatement :: Parser Statement 
pIfElseStatement = do
    reserved "if"
    condition <- pExpression
    reserved "then"
    body <- many1 pStatement <?> "statement, \"else\" or \"fi\""
    reserved "else"
    elseBody <- many1 pStatement
    reserved "fi"
    return $ SIfElse condition body elseBody

-- | Parses a while statement and returns a Statement node if accepted 
pWhileStatement :: Parser Statement 
pWhileStatement = do
    reserved "while"
    condition <- pExpression 
    reserved "do"
    body <- many1 pStatement <?> "statement or \"od\""
    reserved "od"
    return $ SWhile condition body

-----------------------------------
-- Expression Parsing
-----------------------------------

-- | Parses an expression and returns an Expression node if accepted 
pExpression :: Parser LocatedExpr
pExpression = 
        buildExpressionParser opTable pFactor
    <?>
        "expression"

-- | Parses a factor and returns and Expression node if accepted 
pFactor :: Parser LocatedExpr
pFactor =
        choice [ try pLambda, parens pExpression, pStringLiteral, pIntLiteral
               , pBoolLiteral, try pFuncCall, pNegatedExpr, pLvalueExpr ]
    <?>
        "expression"

-- | Operator table for buildExpressionParser that specifies operator precedence
-- reverse because the specification lists them backwards
opTable = reverse
    [ [ binary "or" BinOr AssocLeft ]
    , [ binary "and" BinAnd AssocLeft ]
    , [ unary  "not" UnNot  ]
    , [ binary "=" BinEq AssocNone, binary "!=" BinNeq AssocNone, binary "<" BinLt AssocNone,
        binary "<=" BinLte AssocNone, binary ">" BinGt AssocNone, binary ">=" BinGte AssocNone ]
    , [ binary "+" BinPlus AssocLeft
      , binary "-" BinMinus AssocLeft ] 
    , [ binary "*" BinTimes AssocLeft
      , binary "/" BinDivide AssocLeft ] ]

-----------------------------------
-- Expression Parsing Helper Functions
-----------------------------------

-- | Parses a negated expression and returns an Expression node if accepted 
-- Handle negated expressions (integer and boolean) separately so that we can handle nested
-- prefix operators, e.g. `b <- not not not ------1;`
pNegatedExpr :: Parser LocatedExpr
pNegatedExpr = do
        pos <- sourcePos
        lexeme $ char '-'
        LocatedExpr pos . EUnOp UnNegate <$> pFactor
    <|> do
        pos <- sourcePos
        reservedOp "not"
        LocatedExpr pos . EUnOp UnNot <$> pFactor

-- | Parses an lvalue expression and returns an Expression node if accepted 
pLvalueExpr :: Parser LocatedExpr
pLvalueExpr = liftSourcePos $ ELvalue <$> pLvalue

-- | Parses an int literal and returns an Expression node if accepted
pIntLiteral :: Parser LocatedExpr
pIntLiteral = liftSourcePos $ EConst . LitInt <$> decimal

-- | Parses a boolean expression and returns an Expression node if accepted 
pBoolLiteral :: Parser LocatedExpr
pBoolLiteral =
        liftSourcePos ((EConst . LitBool) <$> (reserved "true"  $> True))
    <|>
        liftSourcePos ((EConst . LitBool) <$> (reserved "false" $> False))

-- | Parses a string literal and returns an Expression node if accepted. Works by
-- seeing if we can parse a valid escape sequence, and if so, returning a string
-- containing the unaltered escape sequence; otherwise, returns a singleton string
-- with the character. This is so that it rejects invalid escape sequences, but
-- doesn't alter them -- for later injection into Oz.
pStringLiteral :: Parser LocatedExpr
pStringLiteral
    = liftSourcePos $ EConst . LitString . T.pack <$> between quote (lexeme quote) pStringLitChars
    where
        pStringLitChars = concat <$> many (choice
            [ char '\\' *> choice [
                pEscapeSequence '"'
                -- \\ is not technically in the spec, but otherwise you can't print certain things
                , pEscapeSequence '\\'
                , pEscapeSequence 'n'
                , pEscapeSequence 't' ] 
            , pure <$> noneOf "\"" ])
    
-- | Parses an escape sequence for the character `escaped`
pEscapeSequence :: Char -> Parser String
pEscapeSequence escaped = char escaped $> ('\\' : [escaped])
    <?> "escape sequence (\\\", \\\\, \\n, or \\t)"

pFuncCall :: Parser LocatedExpr
pFuncCall = liftSourcePos $ liftA2 EFunc pIdent (parens (pExpression `sepBy` comma))

-- | Hacky way to parse "lambda" without making it a reserved keyword.
pLambdaKeyword :: Parser ()
pLambdaKeyword = do
    lexeme $ char 'l'
    char 'a'
    char 'm'
    char 'b'
    char 'd'
    lexeme $ char 'a'
    return ()

pLambda :: Parser LocatedExpr
pLambda = do
    pos <- sourcePos
    pLambdaKeyword
    params <- parens (pFormalParam `sepBy` comma)
    reservedOp "->"
    retType <- option VoidTypeName (PrimitiveTypeName <$> pPrimitiveType)
    varDecls <- many pVarDecl
    statements <- braces (many pStatement);
    return $ LocatedExpr pos $ ELambda params retType varDecls statements

-- | Composes operators to allow unary operator chaining when parsing expressions
-- Here is some black magic: we turn the problem of parsing "not" into a function that
-- turns the next expression we see into a Boolean-negated one. Although we have "not"
-- included in the negated expression factor above, this will mess up precedence -- so
-- we also introduce it part of the op table below.
-- 
-- The below code tells Parsec to make a prefix operator by simply composing the "not"
-- prefix operator parser with whatever it produces next. This allows operator chaining.
composeOpParser :: Parser ((b -> c) -> (a -> b) -> a -> c)
composeOpParser = return (.)

-- | Create an unary operator parser from a name and a syntax tree node
unary name op = Prefix $
    chainl1 (reserved name $>
        (\expr -> let pos = locate expr in
            LocatedExpr pos $ EUnOp op expr)
     ) composeOpParser

-- | Create a binary operator parser from a name and a syntax tree node
binary name op = Infix $
    try $ reservedOp name $>
        (\lhs rhs -> let pos = locate rhs in
            LocatedExpr pos $ EBinOp op lhs rhs)
