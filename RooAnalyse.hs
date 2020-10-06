module RooAnalyse where

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

import Common

import RooAst
import RooPrettyPrinter (prettyBinOp)

-- | An expression paired with its type.
data TypedExpression = TypedExpression Type Expression

typeOf :: TypedExpression -> Type
typeOf (TypedExpression ty _) = ty

-- | Type-checks a located expression, and returns the expression annotated with its type
--   if successful. This is a helpful shortcut for code generation.
analyseExpression :: LocatedExpr -> Either [AnalysisError] TypedExpression
analyseExpression expr = liftSingleErr $ (typecheckExpression . fromLocated) expr

-- | Type-checks an expression, and returns the expression annotated with its type
--   if successful.
typecheckExpression :: Expression -> Either AnalysisError TypedExpression
typecheckExpression (ELvalue _) = error "not yet implemented"

-- Literals are always well-typed.
typecheckExpression expr@(EConst literal) = Right $ case literal of
    LitBool   _ -> TypedExpression TBool expr
    LitInt    _ -> TypedExpression TInt expr
    LitString _ -> TypedExpression TString expr

-- Boolean negations must check whether the inner expression is boolean.
typecheckExpression expr@(EUnOp UnNot (LocatedExpr pos inner)) = do
    exprType <- typeOf <$> typecheckExpression inner
    case exprType of
        TBool -> return $ TypedExpression TInt expr
        ty    -> fromSourcePos pos $ "expecting `boolean`, found `" ++ show ty ++ "`"

-- Integer negations must check whether the inner expression is an integer.
typecheckExpression expr@(EUnOp UnNegate (LocatedExpr pos inner)) = do
    exprType <- typeOf <$> typecheckExpression inner
    case exprType of
        TInt -> return $ TypedExpression TInt expr
        ty   -> fromSourcePos pos $ "expecting `integer`, found `" ++ show ty ++ "`"

-- For binary expressions there are three cases:
--  1. The operator is a boolean operator (in which case both sides must be boolean)
--  2. The operator is an integer operator (in which case both sides must be integers)
--  3. The operator is a comparison operator (in which case both sides must be
--     of the same non-string type)
typecheckExpression expr@(EBinOp op (LocatedExpr lPos lhs) (LocatedExpr rPos rhs))
    | op `elem` [BinOr, BinAnd]                          = checkBoth TBool
    | op `elem` [BinPlus, BinMinus, BinTimes, BinDivide] = checkBoth TInt
    | otherwise = do
        lExpr <- typecheckExpression lhs
        rExpr <- typecheckExpression rhs
        let ltype = typeOf lExpr
        let rtype = typeOf rExpr
        if ltype /= TString then
            if rtype /= TString then
                if ltype == rtype then
                    return $ TypedExpression ltype expr
                else
                    fromSourcePos lPos $ concat
                        [ "operands do not match: `"
                        , show ltype
                        , "` vs `"
                        , show rtype
                        , "`" ]
            else
                fromSourcePos rPos $ "cannot compare `string`"
        else
            fromSourcePos lPos $ "cannot compare `string`"
    where
        checkBoth ty = do
            ltype <- typeOf <$> typecheckExpression lhs
            rtype <- typeOf <$> typecheckExpression rhs
            if ltype == ty then
                if rtype == ty then
                    return $ TypedExpression ty expr
                else
                    fromSourcePos rPos $ concat
                        [ "expecting `"
                        , show ty
                        , "` on RHS of `"
                        , prettyBinOp op
                        , "`, found `"
                        , show rtype
                        , "`" ]
            else
                fromSourcePos lPos $ concat
                 [ "expecting `"
                 , show ty
                 , "` on LHS of `"
                 , prettyBinOp op
                 , "`, found `"
                 , show ltype
                 , "`" ]
