module RooAnalyse where

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

import RooAst
import RooPrettyPrinter (prettyBinOp)

data AnalysisError = AnalysisError Int Int String

fromSourcePos :: SourcePos -> String -> AnalysisError
fromSourcePos pos err = AnalysisError (sourceLine pos) (sourceColumn pos) err

combineErrors :: Foldable t => b -> t (Either [a] b) -> Either [a] b
combineErrors initial = foldr combine (Right initial)
    where
        combine (Right val)  (Right _) = Right val
        combine (Left errs) (Right _) = Left errs
        combine (Left errs) (Left list) = Left $ errs ++ list
        combine _          (Left list) = Left list

mapErr :: (a -> c) -> Either a b -> Either c b
mapErr f (Left err) = Left (f err)
mapErr _ (Right val) = Right val

unwrapOr :: (a -> c) -> Either a b -> Either c ()
unwrapOr f (Left err) = Left (f err)
unwrapOr _ _ = Right ()

data Type = TBool | TString | TInt
    deriving Eq

instance Show Type where
    show TBool   = "boolean"
    show TString = "string"
    show TInt    = "integer"

data TypedStatement = TypedStatement 

data TypedExpression = TypedExpression Type Expression

typeOf :: TypedExpression -> Type
typeOf (TypedExpression ty _) = ty

analyseExpression :: LocatedExpr -> Either [AnalysisError] TypedExpression
analyseExpression expr = mapErr pure $ (typecheckExpression . fromLocated) expr

typecheckExpression :: Expression -> Either AnalysisError TypedExpression
typecheckExpression (ELvalue _) = error "not yet implemented"

typecheckExpression expr@(EConst literal) = case literal of
    LitBool   _ -> Right $ TypedExpression TBool expr
    LitInt    _ -> Right $ TypedExpression TInt expr
    LitString _ -> Right $ TypedExpression TString expr

typecheckExpression expr@(EUnOp UnNot (LocatedExpr pos inner)) = do
    exprType <- typeOf <$> typecheckExpression inner
    case exprType of
        TBool -> Right $ TypedExpression TInt expr
        ty    -> Left $ fromSourcePos pos $ "expecting `boolean`, found `" ++ show ty ++ "`"

typecheckExpression expr@(EUnOp UnNegate (LocatedExpr pos inner)) = do
    exprType <- typeOf <$> typecheckExpression inner
    case exprType of
        TInt -> return $ TypedExpression TInt expr
        ty   -> Left $ fromSourcePos pos $ "expecting `integer`, found `" ++ show ty ++ "`"

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
                    Left $ fromSourcePos lPos $ "operands do not match: `" ++ show ltype ++ "` vs `" ++ show rtype ++ "`"
            else
                Left $ fromSourcePos lPos $ "cannot compare `string`"
        else
            Left $ fromSourcePos lPos $ "cannot compare `string`"
    where
        checkBoth ty = do
            ltype <- typeOf <$> typecheckExpression lhs
            rtype <- typeOf <$> typecheckExpression rhs
            if ltype == ty then
                if rtype == ty then
                    return $ TypedExpression ty expr
                else
                    Left $ fromSourcePos rPos $ "expecting `" ++ show ty ++ "` on RHS of `" ++ prettyBinOp op ++ "`, found `" ++ show rtype ++ "`"
            else
                Left $ fromSourcePos lPos $ "expecting `" ++ show ty ++ "` on LHS of `" ++ prettyBinOp op ++ "`, found `" ++ show ltype ++ "`"
