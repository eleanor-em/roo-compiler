{-# LANGUAGE OverloadedStrings #-}

module RooAnalyse where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Text.Parsec (SourcePos)

import Common

import RooAst
import RooPrettyPrinter (prettyBinOp)
import SymTable
import Control.Monad (unless)

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
analyseExpression :: RootTable -> LocalTable -> LocatedExpr -> Either [AnalysisError] TypedExpr
analyseExpression table locals expr = typecheckExpression table locals $ fromLocated expr

-- | Type-checks an expression, and ensures it is of integer type.
typecheckArrayIndex :: RootTable -> LocalTable -> LocatedExpr -> Either [AnalysisError] ()
typecheckArrayIndex table locals (LocatedExpr pos expr) = do
    indexTy <- typecheckExpression table locals expr
    case typeof indexTy of
        TInt -> return ()
        ty -> Left $ errorPos pos $
            "expected index expression of type `integer`, found `" <> tshow ty <> "`"

-- | Type-checks an expression, and returns the expression annotated with its type
--   if successful.
typecheckExpression :: RootTable -> LocalTable -> Expression -> Either [AnalysisError] TypedExpr
typecheckExpression _ locals expr@(ELvalue (LId (Ident pos ident)))
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> pure $ TypedExpr (procSymType $ symType sym) expr
        Nothing  -> Left $ errorPos pos $ "in expression: unknown variable `" <> ident <> "`"

typecheckExpression table locals expr@(ELvalue lvalue) = do
    ty <- analyseLvalue table locals lvalue
    return $ TypedExpr (lvalueType ty) expr

-- Literals are always well-typed.
typecheckExpression _ _ expr@(EConst literal) = Right $ case literal of
    LitBool   _ -> TypedExpr TBool   expr
    LitInt    _ -> TypedExpr TInt    expr
    LitString _ -> TypedExpr TString expr

-- Boolean negations must check whether the inner expression is boolean.
typecheckExpression table locals expr@(EUnOp UnNot (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression table locals inner
    case exprType of
        TBool -> pure $ TypedExpr TBool expr
        ty    -> Left $ errorPos pos $ "expecting `boolean`, found `" <> tshow ty <> "`"

-- Integer negations must check whether the inner expression is an integer.
typecheckExpression table locals expr@(EUnOp UnNegate (LocatedExpr pos inner)) = do
    exprType <- typeof <$> typecheckExpression table locals inner
    case exprType of
        TInt -> pure $ TypedExpr TInt expr
        ty   -> Left $ errorPos pos $ "expecting `integer`, found `" <> tshow ty <> "`"

-- For binary expressions there are three cases:
--  1. The operator is a boolean operator (in which case both sides must be boolean)
--  2. The operator is an integer operator (in which case both sides must be integers)
--  3. The operator is a comparison operator (in which case both sides must be
--     of the same non-string type)
typecheckExpression table locals expr@(EBinOp op (LocatedExpr lPos lhs) (LocatedExpr rPos rhs))
    | op `elem` [BinOr, BinAnd]                                 = checkBoth TBool
    | op `elem` [BinLt, BinLte, BinEq, BinNeq, BinGt, BinGte]   = do
        ltype <- typeof <$> typecheckExpression table locals lhs
        rtype <- typeof <$> typecheckExpression table locals rhs
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
        ltype <- typeof <$> typecheckExpression table locals lhs
        rtype <- typeof <$> typecheckExpression table locals rhs
        
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
            ltype <- typeof <$> typecheckExpression table locals lhs
            rtype <- typeof <$> typecheckExpression table locals rhs
            
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

typecheckExpression table locals expr@(EFunc func args) = do
    (_, _, ty) <- typecheckCall table locals func args
    return $ TypedExpr ty expr

typecheckCall :: RootTable -> LocalTable -> Ident -> [LocatedExpr]
                           -> Either [AnalysisError] ([TypedExpr], [ProcSymbol], Type)
typecheckCall table locals (Ident pos name) args = do
    (targetPos, targetProc) <- unwrapOr (lookupProc table name)
                                        (Left $ errorPos pos $ "unknown procedure `" <> name <> "`")
    let params = localParams targetProc

    -- Check # arguments = # parameters
    unless  (length args == length params)
            (let err  = mconcat
                    [ "`", name, "` expects ", countWithNoun (length params) "parameter"
                    , " but was given ", tshow (length args) ]
                 note = "`" <> name <> "` declared here:" in
                Left $ errorWithNote pos err targetPos note)

    -- Type-check arguments
    typedArgs <- concatEither $ map ((pure <$>) . analyseExpression table locals)
                                    args

    -- Check argument types match parameter types
    let mismatched = filter (\((_, a), b) -> typeof a /= rawSymType b)
                            (zip (enumerate typedArgs) params)

    let reportErr ((i, expr), symbol) = let err  = mconcat [ "in argument: expecting `"
                                                , tshow $ rawSymType symbol
                                                , "`, found `"
                                                , tshow $ typeof expr
                                                , "`" ]
                                            note = "parameter declared here:"  in
            Left $ errorWithNote (locate $ args !! i) err (symPos symbol) note

    concatEither $ map reportErr mismatched

    -- Check reference args are filled with lvalues
    let mismatched = filter (\((_, a), b) -> not (checkRefArgs a b))
                            (zip (enumerate typedArgs) params)

    let reportErr ((i, _), symbol) = let err  = mconcat [ "in argument: expecting lvalue" ]
                                         note = "parameter declared here:" in
            Left $ errorWithNote (locate $ args !! i) err (symPos symbol) note

    concatEither $ map reportErr mismatched

    return (typedArgs, params, localRetType targetProc)
    
    where
        -- | Checks whether the expression and symbol are either:
        --   lvalue + ref
        --   any + val
        checkRefArgs (TypedExpr _ (ELvalue _)) (ProcSymbol (RefSymbol _) _ _ _) = True
        checkRefArgs _ (ProcSymbol (RefSymbol _) _ _ _) = False
        checkRefArgs _ _ = True

simplifyExpression :: Expression -> Expression
simplifyExpression (EUnOp UnNot inner)
    = case simplifyExpression (fromLocated inner) of
        EConst (LitBool val) -> EConst (LitBool (not val))
        expr -> EUnOp UnNot (liftExpr expr)

simplifyExpression (EUnOp UnNegate inner)
    = case simplifyExpression (fromLocated inner) of
        EConst (LitInt val) -> EConst (LitInt (-val))
        expr -> EUnOp UnNegate (liftExpr expr)

simplifyExpression (EBinOp BinAnd lhs rhs)
    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        -- As per Piazza, eliminate below short-circuiting. :(
            
        -- (EConst (LitBool False), _) -> EConst (LitBool False)
        -- (_ , EConst (LitBool False)) -> EConst (LitBool False)
        (EConst (LitBool lhs), EConst (LitBool rhs)) -> EConst (LitBool (lhs && rhs))
        (lhs, rhs) -> EBinOp BinAnd (liftExpr lhs) (liftExpr rhs)

simplifyExpression (EBinOp BinOr lhs rhs)
    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        -- As per Piazza, eliminate below short-circuiting. :(
            
        -- (EConst (LitBool True), _) -> EConst (LitBool True)
        -- (_ , EConst (LitBool True)) -> EConst (LitBool True)
        (EConst (LitBool lhs), EConst (LitBool rhs)) -> EConst (LitBool (lhs || rhs))
        (lhs, rhs) -> EBinOp BinOr (liftExpr lhs) (liftExpr rhs)

simplifyExpression (EBinOp BinPlus lhs rhs)
    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        (EConst (LitInt lhs),  EConst (LitInt rhs))  -> EConst (LitInt (lhs + rhs))
        (lhs, rhs) -> EBinOp BinPlus (liftExpr lhs) (liftExpr rhs)

simplifyExpression (EBinOp BinMinus lhs rhs)
    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        (EConst (LitInt lhs),  EConst (LitInt rhs))  -> EConst (LitInt (lhs - rhs))
        (lhs, rhs) -> EBinOp BinMinus (liftExpr lhs) (liftExpr rhs)

simplifyExpression (EBinOp BinTimes lhs rhs)
    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        (EConst (LitInt lhs),  EConst (LitInt rhs))  -> EConst (LitInt (lhs * rhs))
        (lhs, rhs) -> EBinOp BinTimes (liftExpr lhs) (liftExpr rhs)

simplifyExpression (EBinOp BinDivide lhs rhs)
    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        expr@(EConst (LitInt lhs),  EConst (LitInt rhs))  -> 
            if rhs /= 0 then
                EConst (LitInt (lhs `div` rhs))
            else
                uncurry (EBinOp BinDivide) (mapPair liftExpr expr)
        (lhs, rhs) -> EBinOp BinDivide (liftExpr lhs) (liftExpr rhs)

-- The only remaining cases are comparison operators
simplifyExpression (EBinOp binop lhs rhs)
    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        (EConst (LitBool lhs), EConst (LitBool rhs)) -> EConst (LitBool (lhs `op` rhs))
        (EConst (LitInt lhs),  EConst (LitInt rhs))  -> EConst (LitBool (lhs `op` rhs))
        (lhs, rhs) -> EBinOp binop (liftExpr lhs) (liftExpr rhs)
    where
        a `op` b = case binop of
            BinLt  -> a < b
            BinLte -> a <= b
            BinGt  -> a > b
            BinGte -> a >= b
            BinNeq -> a /= b
            _      -> a == b -- this will only match BinEq

simplifyExpression expr = expr

data TypedLvalue = TypedRefLvalue Type StackSlot Expression Text SourcePos
                 | TypedValLvalue Type StackSlot Expression Text SourcePos

lvalueType :: TypedLvalue -> Type
lvalueType (TypedRefLvalue ty _ _ _ _) = ty
lvalueType (TypedValLvalue ty _ _ _ _) = ty

lvalueLocation :: TypedLvalue -> StackSlot
lvalueLocation (TypedRefLvalue _ loc _ _ _) = loc
lvalueLocation (TypedValLvalue _ loc _ _ _) = loc

lvalueOffset :: TypedLvalue -> Expression
lvalueOffset (TypedRefLvalue _ _ off _ _) = off
lvalueOffset (TypedValLvalue _ _ off _ _) = off

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

analyseLvalue :: RootTable -> LocalTable -> Lvalue -> Either [AnalysisError] TypedLvalue
analyseLvalue _ locals (LId (Ident pos name)) = do
    sym <- unwrapOr (Map.lookup name $ localSymbols locals)
                    (Left $ errorPos pos $ "in statement: unknown variable `" <> name <> "`")
    return $ symToTypedLvalue sym noOffset

analyseLvalue table locals (LArray (Ident pos ident) indexExpr)
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> do
            case procSymType $ symType sym of
                TArray _ _ ty -> do
                    typecheckArrayIndex table locals indexExpr

                    let pos = locate indexExpr
                    index <- if sizeof ty /= 1 then
                        return $ EBinOp BinTimes
                                        (LocatedExpr pos (EConst (LitInt (sizeof ty))))
                                        indexExpr
                    else
                        return $ simplifyExpression (fromLocated indexExpr)

                    return $ symToTypedLvalue
                        (ProcSymbol (cons sym ty) (symLocation sym) pos (ident <> "[]"))
                        index

                ty -> Left $ errorPos pos $ "expected array type, found `" <> tshow ty <> "`"
        Nothing  -> Left $ errorPos pos $ "in expression: unknown variable `" <> ident <> "`"
    where
        cons ty = case symType ty of
            ValSymbol _ -> ValSymbol
            RefSymbol _ -> RefSymbol

analyseLvalue _ locals (LMember (Ident recPos recName) (Ident fldPos fldName)) = do
    recSym <- unwrapOr (Map.lookup recName $ localSymbols locals)
                       (Left $ errorPos recPos $ "in statement: unknown variable `"
                                              <> recName <> "`")

    let ty = rawSymType recSym

    case ty of
        TRecord _ fieldMap -> do
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
                TArray _ _ ty@(TRecord _ fields) -> do
                    typecheckArrayIndex symbols locals indexExpr
                    case Map.lookup fldName fields of
                        Just (Field _ offset innerTy) -> do
                            let pos = locate indexExpr
                            index <- if sizeof ty /= 1 then
                                return $ EBinOp BinTimes
                                                (LocatedExpr pos (EConst (LitInt (sizeof ty))))
                                                indexExpr
                            else
                                return $ simplifyExpression (fromLocated indexExpr)

                            return $ symToTypedLvalue
                                (ProcSymbol (cons sym innerTy)
                                            (symLocation sym)
                                            pos
                                            (arrName <> "[]" <> fldName))
                                (EBinOp BinPlus
                                        (LocatedExpr pos ((EConst . LitInt . stackSlotToInt) offset))
                                        (LocatedExpr pos index))
                        _ -> Left $ errorPos fldPos $ "in expression: unknown field name `"
                                                   <> fldName <> "`"

                ty -> Left $ errorPos arrPos $ "expected array of records, found `"
                                            <> tshow ty <> "`"
        Nothing  -> Left $ errorPos arrPos $ "in expression: unknown variable `"
                                          <> arrName <> "`"
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

