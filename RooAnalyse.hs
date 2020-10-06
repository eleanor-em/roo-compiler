module RooAnalyse where

import qualified Data.Map.Strict as Map

import Common

import RooAst
import RooPrettyPrinter (prettyBinOp)
import SymTable

-- | Checks if the symbol table contains an appropriate main procedure.
hasMain :: RootTable -> Bool
hasMain symbols = case Map.lookup "main" (rootProcs symbols) of
    Just (_, prc) -> null (localParams prc)
    Nothing       -> False

-- | An expression paired with its type.
data TypedExpr = TypedExpr Type Expression

-- | Extracts the type of a TypedExpr.
typeof :: TypedExpr -> Type
typeof (TypedExpr ty _) = ty

-- | Type-checks a located expression, and returns the expression annotated with its type
--   if successful. This is a helpful shortcut for code generation.
analyseExpression :: AliasTable -> LocalTable -> LocatedExpr -> Either [AnalysisError] TypedExpr
analyseExpression aliases locals expr = typecheckExpression aliases locals $ fromLocated expr

-- | Type-checks an expression, and ensures it is of integer type.
typecheckArrayIndex :: AliasTable -> LocalTable -> LocatedExpr -> Either [AnalysisError] ()
typecheckArrayIndex aliases locals (LocatedExpr pos expr) = do
    indexTy <- typecheckExpression aliases locals expr
    case typeof indexTy of
        TInt -> return ()
        ty -> liftOne $ errorPos pos $
            "expected index expression of type `integer`, found `" <> show ty <> "`"

-- | Type-checks an expression, and returns the expression annotated with its type
--   if successful.
typecheckExpression :: AliasTable -> LocalTable -> Expression -> Either [AnalysisError] TypedExpr
typecheckExpression _ locals expr@(ELvalue (LId (Ident pos ident)))
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> Right $ TypedExpr (procSymType $ symType sym) expr
        Nothing  -> liftOne $ errorPos pos $ "unknown identifier `" <> ident <> "`"

typecheckExpression aliases locals expr@(ELvalue (LArray (Ident pos ident) indexExpr))
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> do
            case procSymType $ symType sym of
                TArray _ ty -> do
                    typecheckArrayIndex aliases locals indexExpr
                    return $ TypedExpr ty expr
                ty -> liftOne $ errorPos pos $ "expected array type, found `" <> show ty <> "`"
        Nothing  -> liftOne $ errorPos pos $ "unknown identifier `" <> ident <> "`"

-- Literals are always well-typed.
typecheckExpression _ _ expr@(EConst literal) = Right $ case literal of
    LitBool   _ -> TypedExpr TBool expr
    LitInt    _ -> TypedExpr TInt expr
    LitString _ -> TypedExpr TString expr

-- Boolean negations must check whether the inner expression is boolean.
typecheckExpression aliases locals expr@(EUnOp UnNot (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression aliases locals inner
    case exprType of
        TBool -> return $ TypedExpr TInt expr
        ty    -> liftOne $ errorPos pos $ "expecting `boolean`, found `" <> show ty <> "`"

-- Integer negations must check whether the inner expression is an integer.
typecheckExpression aliases locals expr@(EUnOp UnNegate (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression aliases locals inner
    case exprType of
        TInt -> return $ TypedExpr TInt expr
        ty   -> liftOne $ errorPos pos $ "expecting `integer`, found `" <> show ty <> "`"

-- For binary expressions there are three cases:
--  1. The operator is a boolean operator (in which case both sides must be boolean)
--  2. The operator is an integer operator (in which case both sides must be integers)
--  3. The operator is a comparison operator (in which case both sides must be
--     of the same non-string type)
typecheckExpression aliases locals expr@(EBinOp op (LocatedExpr lPos lhs) (LocatedExpr rPos rhs))
    | op `elem` [BinOr, BinAnd]                          = checkBoth TBool
    | op `elem` [BinPlus, BinMinus, BinTimes, BinDivide] = checkBoth TInt
    | otherwise = do
        ltype <- typeof <$> typecheckExpression aliases locals lhs
        rtype <- typeof <$> typecheckExpression aliases locals rhs
        
        if ltype /= TString then
            if rtype /= TString then
                if ltype == rtype then
                    return $ TypedExpr ltype expr
                else
                    liftOne $ errorPos lPos $ concat
                        [ "operands do not match: `"
                        , show ltype
                        , "` vs `"
                        , show rtype
                        , "`" ]
            else
                liftOne $ errorPos rPos "cannot compare `string`"
        else
            liftOne $ errorPos lPos "cannot compare `string`"
    where
        checkBoth ty = do
            ltype <- typeof <$> typecheckExpression aliases locals lhs
            rtype <- typeof <$> typecheckExpression aliases locals rhs
            
            if ltype == ty then
                if rtype == ty then
                    return $ TypedExpr ty expr
                else
                    liftOne $ errorPos rPos $ concat
                        [ "expecting `"
                        , show ty
                        , "` on RHS of `"
                        , prettyBinOp op
                        , "`, found `"
                        , show rtype
                        , "`" ]
            else
                liftOne $ errorPos lPos $ concat
                 [ "expecting `"
                 , show ty
                 , "` on LHS of `"
                 , prettyBinOp op
                 , "`, found `"
                 , show ltype
                 , "`" ]

typecheckExpression _ _ _ = error "typecheckExpression: not yet implemented"
