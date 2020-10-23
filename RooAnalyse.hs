{-# LANGUAGE OverloadedStrings #-}

module RooAnalyse where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Text.Parsec (SourcePos)

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
        ty -> Left $ errorPos pos $
            "expected index expression of type `integer`, found `" <> tshow ty <> "`"

-- | Type-checks an expression, and returns the expression annotated with its type
--   if successful.
typecheckExpression :: AliasTable -> LocalTable -> Expression -> Either [AnalysisError] TypedExpr
typecheckExpression _ locals expr@(ELvalue (LId (Ident pos ident)))
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> pure $ TypedExpr (procSymType $ symType sym) expr
        Nothing  -> Left $ errorPos pos $ "in expression: unknown variable `" <> ident <> "`"

typecheckExpression aliases locals expr@(ELvalue lvalue) = do
    ty <- analyseLvalue aliases locals lvalue
    return $ TypedExpr (lvalueType ty) expr

-- Literals are always well-typed.
typecheckExpression _ _ expr@(EConst literal) = Right $ case literal of
    LitBool   _ -> TypedExpr TBool   expr
    LitInt    _ -> TypedExpr TInt    expr
    LitString _ -> TypedExpr TString expr

-- Boolean negations must check whether the inner expression is boolean.
typecheckExpression aliases locals expr@(EUnOp UnNot (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression aliases locals inner
    case exprType of
        TBool -> pure $ TypedExpr TBool expr
        ty    -> Left $ errorPos pos $ "expecting `boolean`, found `" <> tshow ty <> "`"

-- Integer negations must check whether the inner expression is an integer.
typecheckExpression aliases locals expr@(EUnOp UnNegate (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression aliases locals inner
    case exprType of
        TInt -> pure $ TypedExpr TInt expr
        ty   -> Left $ errorPos pos $ "expecting `integer`, found `" <> tshow ty <> "`"

-- For binary expressions there are three cases:
--  1. The operator is a boolean operator (in which case both sides must be boolean)
--  2. The operator is an integer operator (in which case both sides must be integers)
--  3. The operator is a comparison operator (in which case both sides must be
--     of the same non-string type)
typecheckExpression aliases locals expr@(EBinOp op (LocatedExpr lPos lhs) (LocatedExpr rPos rhs))
    | op `elem` [BinOr, BinAnd]                                 = checkBoth TBool
    | op `elem` [BinLt, BinLte, BinEq, BinNeq, BinGt, BinGte]   = do
        ltype <- typeof <$> typecheckExpression aliases locals lhs
        rtype <- typeof <$> typecheckExpression aliases locals rhs
        if ltype == rtype then
            pure    $ TypedExpr TBool expr
        else
            Left $ errorPos lPos $ mconcat
                [ "operands do not match: `"
                , tshow ltype
                , "` vs `"
                , tshow rtype
                , "`" ]
        
    | op `elem` [BinPlus, BinMinus, BinTimes, BinDivide]        = checkBoth TInt
    | otherwise = do
        ltype <- typeof <$> typecheckExpression aliases locals lhs
        rtype <- typeof <$> typecheckExpression aliases locals rhs
        
        if ltype /= TString then
            if rtype /= TString then
                if ltype == rtype then
                    pure    $ TypedExpr ltype expr
                else
                    Left $ errorPos lPos $ mconcat
                        [ "operands do not match: `"
                        , tshow ltype
                        , "` vs `"
                        , tshow rtype
                        , "`" ]
            else
                Left $ errorPos rPos "cannot compare `string`"
        else
            Left $ errorPos lPos "cannot compare `string`"
    where
        checkBoth ty = do
            ltype <- typeof <$> typecheckExpression aliases locals lhs
            rtype <- typeof <$> typecheckExpression aliases locals rhs
            
            if ltype == ty then
                if rtype == ty then
                    pure    $ TypedExpr ty expr
                else
                    Left $ errorPos rPos $ mconcat
                        [ "expecting `"
                        , tshow ty
                        , "` on RHS of `"
                        , prettyBinOp op
                        , "`, found `"
                        , tshow rtype
                        , "`" ]
            else
                Left $ errorPos lPos $ mconcat
                 [ "expecting `"
                 , tshow ty
                 , "` on LHS of `"
                 , prettyBinOp op
                 , "`, found `"
                 , tshow ltype
                 , "`" ]

data TypedLvalue = TypedRefLvalue Type StackSlot Expression Text SourcePos
                 | TypedValLvalue Type StackSlot Expression Text SourcePos

lvalueType :: TypedLvalue -> Type
lvalueType (TypedRefLvalue ty _ _ _ _) = ty
lvalueType (TypedValLvalue ty _ _ _ _) = ty

lvalueName :: TypedLvalue -> Text
lvalueName (TypedRefLvalue _ _ _ name _) = name
lvalueName (TypedValLvalue _ _ _ name _) = name

lvaluePos :: TypedLvalue -> SourcePos
lvaluePos (TypedRefLvalue _ _ _ _ pos) = pos
lvaluePos (TypedValLvalue _ _ _ _ pos) = pos

symToTypedLvalue :: ProcSymbol -> Expression -> TypedLvalue
symToTypedLvalue (ProcSymbol (RefSymbol ty) slot pos name) offset
    = TypedRefLvalue ty slot offset name pos

symToTypedLvalue (ProcSymbol (ValSymbol ty) slot pos name) offset
    = TypedValLvalue ty slot offset name pos

noOffset :: Expression
noOffset = EConst (LitInt 0)

analyseLvalue :: AliasTable -> LocalTable -> Lvalue -> Either [AnalysisError] TypedLvalue
analyseLvalue _ locals (LId (Ident pos name)) = do
    sym <- unwrapOr (Map.lookup name $ localSymbols locals)
                    (Left $ errorPos pos $ "in statement: unknown variable `" <> name <> "`")
    return $ symToTypedLvalue sym noOffset

analyseLvalue symbols locals (LArray (Ident pos ident) indexExpr)
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> do
            case procSymType $ symType sym of
                TArray _ ty -> do
                    typecheckArrayIndex symbols locals indexExpr

                    return $ symToTypedLvalue
                        (ProcSymbol (cons sym ty)  (symLocation sym) pos ident)
                        (fromLocated indexExpr)

                ty -> Left $ errorPos pos $ "expected array type, found `" <> tshow ty <> "`"
        Nothing  -> Left $ errorPos pos $ "in expression: unknown variable `" <> ident <> "`"
    where
        cons ty = case symType ty of
            ValSymbol _ -> ValSymbol
            RefSymbol _ -> RefSymbol

analyseLvalue _ locals (LMember (Ident recPos recName) (Ident fldPos fldName)) = do
    recSym <- unwrapOr (Map.lookup recName $ localSymbols locals)
                       (Left $ errorPos recPos $ "in statement: unknown variable `" <> recName <> "`")

    let ty = rawSymType recSym

    case ty of
        TRecord fieldMap -> do
            fieldSym <- unwrapOr (Map.lookup fldName fieldMap)
                                 (Left $ errorWithNote
                                    fldPos ("in statement: unknown field `" <> recName <> "`")
                                    (symPos recSym) "record declared here:")
            -- If the record is of value kind, we can compute the offset statically.
            -- Otherwise, we must compute it dynamically.
            let (location, offset) = if isValSymbol recSym then
                    (symLocation recSym + fieldOffset fieldSym, noOffset)
                else
                    (symLocation recSym, (EConst . LitInt . stackSlotToInt . fieldOffset) fieldSym)

            cons <- case symType recSym of
                RefSymbol _ -> return TypedRefLvalue
                ValSymbol _ -> return TypedValLvalue

            return $ cons (fieldTy fieldSym) location offset (recName <> "." <> fldName) fldPos

        _ -> Left $ errorPos recPos $ "expected variable of record type, found `" <> tshow ty <> "`"

analyseLvalue symbols locals (LArrayMember (Ident arrPos arrName) indexExpr (Ident fldPos fldName)) 
    = case Map.lookup arrName (localSymbols locals) of
        Just sym -> do
            case procSymType $ symType sym of
                TArray _ ty@(TRecord fields) -> do
                    typecheckArrayIndex symbols locals indexExpr
                    case Map.lookup fldName fields of
                        Just (Field _ offset innerTy) -> do
                            let pos = locate indexExpr
                            index <- if sizeof ty /= 1 then
                                return $ EBinOp BinTimes (LocatedExpr pos (EConst (LitInt (sizeof ty)))) indexExpr
                            else
                                return $ fromLocated indexExpr

                            return $ symToTypedLvalue
                                (ProcSymbol (cons sym innerTy) (symLocation sym) pos (arrName <> "[]" <> fldName))
                                (EBinOp BinPlus (LocatedExpr pos ((EConst . LitInt .stackSlotToInt) offset))
                                                (LocatedExpr pos index))
                        _ -> Left $ errorPos fldPos $ "in expression: unknown field name `" <> fldName <> "`"

                ty -> Left $ errorPos arrPos $ "expected array of records, found `" <> tshow ty <> "`"
        Nothing  -> Left $ errorPos arrPos $ "in expression: unknown variable `" <> arrName <> "`"
    where
        cons ty = case symType ty of
            ValSymbol _ -> ValSymbol
            RefSymbol _ -> RefSymbol

-- | Used to detect possible infinite loops.
lvaluesOf :: Expression -> [Lvalue]
lvaluesOf (ELvalue lval) = [lval]
lvaluesOf (EUnOp _ expr) = lvaluesOf (fromLocated expr)
lvaluesOf (EBinOp _ lhs rhs) = lvaluesOf (fromLocated lhs) <> lvaluesOf (fromLocated rhs)
lvaluesOf _ = []

modifiesLvalue :: Lvalue -> Statement -> Bool
modifiesLvalue lval (SAssign lval' _)
    = nameLvalue lval == nameLvalue lval'

modifiesLvalue lval (SRead lval')
    = nameLvalue lval == nameLvalue lval'

modifiesLvalue lval (SCall _ exprs)
    = any (\expr -> lval `elem` lvaluesOf (fromLocated expr)) exprs

modifiesLvalue lval (SIf _ body)
    = any (modifiesLvalue lval) body

modifiesLvalue lval (SIfElse _ bodyIf bodyElse)
    = any (modifiesLvalue lval) bodyIf || any (modifiesLvalue lval) bodyElse

modifiesLvalue lval (SWhile _ body)
    = any (modifiesLvalue lval) body

modifiesLvalue _ _ = False
