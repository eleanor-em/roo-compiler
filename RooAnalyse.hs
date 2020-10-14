{-# LANGUAGE OverloadedStrings #-}

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
data TypedExpr = TypedExpr 
    { typeof :: Type
    , innerExp :: Expression }

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
            "expected index expression of type `integer`, found `" <> tshow ty <> "`"

-- | Type-checks an expression, and returns the expression annotated with its type
--   if successful.
typecheckExpression :: AliasTable -> LocalTable -> Expression -> Either [AnalysisError] TypedExpr
typecheckExpression _ locals expr@(ELvalue (LId (Ident pos ident)))
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> pure    $ TypedExpr (procSymType $ symType sym) expr
        Nothing  -> liftOne $ errorPos pos $ "in expression: unknown variable `" <> ident <> "`"

typecheckExpression aliases locals expr@(ELvalue (LArray (Ident pos ident) indexExpr))
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> do
            case procSymType $ symType sym of
                TArray _ ty -> do
                    typecheckArrayIndex aliases locals indexExpr
                    pure $ TypedExpr ty expr
                ty -> liftOne $ errorPos pos $ "expected array type, found `" <> tshow ty <> "`"
        Nothing  -> liftOne $ errorPos pos $ "in expression: unknown variable `" <> ident <> "`"

-- Literals are always well-typed.
typecheckExpression _ _ expr@(EConst literal) = Right $ case literal of
    LitBool   _ -> TypedExpr TBool   expr
    LitInt    _ -> TypedExpr TInt    expr
    LitString _ -> TypedExpr TString expr

-- Boolean negations must check whether the inner expression is boolean.
typecheckExpression aliases locals expr@(EUnOp UnNot (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression aliases locals inner
    case exprType of
        TBool -> pure    $ TypedExpr TBool expr
        ty    -> liftOne $ errorPos pos $ "expecting `boolean`, found `" <> tshow ty <> "`"

-- Integer negations must check whether the inner expression is an integer.
typecheckExpression aliases locals expr@(EUnOp UnNegate (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression aliases locals inner
    case exprType of
        TInt -> pure    $ TypedExpr TInt expr
        ty   -> liftOne $ errorPos pos $ "expecting `integer`, found `" <> tshow ty <> "`"

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
                    pure    $ TypedExpr ltype expr
                else
                    liftOne $ errorPos lPos $ mconcat
                        [ "operands do not match: `"
                        , tshow ltype
                        , "` vs `"
                        , tshow rtype
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
                    pure    $ TypedExpr ty expr
                else
                    liftOne $ errorPos rPos $ mconcat
                        [ "expecting `"
                        , tshow ty
                        , "` on RHS of `"
                        , prettyBinOp op
                        , "`, found `"
                        , tshow rtype
                        , "`" ]
            else
                liftOne $ errorPos lPos $ mconcat
                 [ "expecting `"
                 , tshow ty
                 , "` on LHS of `"
                 , prettyBinOp op
                 , "`, found `"
                 , tshow ltype
                 , "`" ]

typecheckExpression _ _ _ = error "typecheckExpression: not yet implemented"

analyseLvalue :: LocalTable -> Lvalue -> Either [AnalysisError] ProcSymbol
analyseLvalue symbols (LId (Ident pos name)) = do
    sym <- unwrapOr (Map.lookup name $ localSymbols symbols)
                    (liftOne $ errorPos pos $ "in statement: unknown variable `" <> name <> "`")
    
    let ty = rawSymType sym

    case ty of
        TArray _ _ -> typeError ty
        TRecord _  -> typeError ty
        _          -> Right sym
    where
        typeError ty = liftOne $ errorPos pos $
            "expected variable of primitive type, found `" <> tshow ty <> "`" 

analyseLvalue _ _ = error "analyseLvalue: not yet implemented"