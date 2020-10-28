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

-----------------------------------
-- Expression Analysis 
-----------------------------------

-- | An expression paired with its type.
data TypedExpr = TypedExpr 
    { exprType :: Type
    , innerExp :: Expression }

-- | Type-checks a located expression, and returns the expression annotated with its type
--   if successful. This is a helpful shortcut for code generation.
analyseExpression :: RootTable -> LocalTable -> LocatedExpr -> Either [AnalysisError] TypedExpr
analyseExpression table locals expr = typecheckExpression table locals $ fromLocated expr

-- | Type-checks an expression, and ensures it is of integer type.
typecheckArrayIndex :: RootTable -> LocalTable -> LocatedExpr -> Either [AnalysisError] ()
typecheckArrayIndex table locals (LocatedExpr pos expr) = do
    indexTy <- typecheckExpression table locals expr
    case exprType indexTy of
        TInt -> return ()
        ty -> Left $ errorPos pos $
            "expected index expression of type `integer`, found " <> backticks ty

-- | Type-checks an expression, and returns the expression annotated with its type
--   if successful.
typecheckExpression :: RootTable -> LocalTable -> Expression -> Either [AnalysisError] TypedExpr
typecheckExpression table locals expr@(ELvalue (LId (Ident pos ident)))
    = case Map.lookup ident (localSymbols locals) of
        Just sym -> pure $ TypedExpr (procSymType $ symType sym) expr
        Nothing  -> case Map.lookup ident (rootFuncPtrs table) of
            Just (_, ty) -> pure $ TypedExpr ty expr
            Nothing      -> Left $ errorPos pos $
                "in expression: unknown variable " <> backticks ident

-- Type checks all the different lvalue variations 
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
    exprType <- exprType <$> typecheckExpression table locals inner
    case exprType of
        TBool -> pure $ TypedExpr TBool expr
        ty    -> Left $ errorPos pos $ "expecting `boolean`, found " <> backticks ty

-- Integer negations must check whether the inner expression is an integer.
typecheckExpression table locals expr@(EUnOp UnNegate (LocatedExpr pos inner)) = do
    exprType <- exprType <$> typecheckExpression table locals inner
    case exprType of
        TInt -> pure $ TypedExpr TInt expr
        ty   -> Left $ errorPos pos $ "expecting `integer`, found " <> backticks ty

-- For binary expressions there are three cases:
--  1. The operator is a boolean operator (in which case both sides must be boolean)
--  2. The operator is an integer operator (in which case both sides must be integers)
--  3. The operator is a comparison operator (in which case both sides must be
--     of the same non-string type)
typecheckExpression table locals expr@(EBinOp op (LocatedExpr lPos lhs) (LocatedExpr rPos rhs))
    | op `elem` [BinOr, BinAnd]                                 = checkBoth TBool
    | op `elem` [BinLt, BinLte, BinEq, BinNeq, BinGt, BinGte]   = do
        ltype <- exprType <$> typecheckExpression table locals lhs
        rtype <- exprType <$> typecheckExpression table locals rhs
        if ltype == rtype then
            pure    $ TypedExpr TBool expr
        else
            Left $ errorPos lPos $ mconcat
                [ "operands do not match: "
                , backticks ltype
                , " vs "
                , backticks rtype ]
    -- We expect arithmetic operations to have integers on both sides.
    | op `elem` [BinPlus, BinMinus, BinTimes, BinDivide]        = checkBoth TInt
    | otherwise = do
        ltype <- exprType <$> typecheckExpression table locals lhs
        rtype <- exprType <$> typecheckExpression table locals rhs
        
        if ltype /= TString then
            if rtype /= TString then
                if ltype == rtype then
                    pure    $ TypedExpr ltype expr
                else
                    Left $ errorPos lPos $ mconcat
                        [ "operands do not match: "
                        , backticks ltype
                        , " vs "
                        , backticks rtype ]
            else
                Left $ errorPos rPos "cannot compare `string`"
        else
            Left $ errorPos lPos "cannot compare `string`"
    where
        checkBoth ty = do
            ltype <- exprType <$> typecheckExpression table locals lhs
            rtype <- exprType <$> typecheckExpression table locals rhs
            
            if ltype == ty then
                if rtype == ty then
                    pure $ TypedExpr ty expr
                else
                    Left $ errorPos rPos $ mconcat
                        [ "expecting "
                        , backticks ty
                        , " on RHS of "
                        , backticks $ prettyBinOp op
                        , ", found "
                        , backticks rtype ]
            else
                Left $ errorPos lPos $ mconcat
                 [ "expecting "
                 , backticks ty
                 , " on LHS of "
                 , backticks $ prettyBinOp op
                 , ", found "
                 , backticks ltype ]

-- For functions, we want to make sure it's a valid function call 
typecheckExpression table locals expr@(EFunc func args) = do
    (_, _, ty) <- typecheckCall table locals func args
    return $ TypedExpr ty expr

-- For lambda expressions, we identify if it returns a primitive or not 
typecheckExpression table _ expr@(ELambda params (LocatedTypeName _ retType) _ _) = do
    paramTys <- mapM (toProcSym table) params
    let retType' = case retType of
            PrimitiveTypeName ty -> liftPrimitive ty
            _                    -> TVoid
    return $ TypedExpr (TFunc paramTys retType') expr

-----------------------------------
-- Type Checking Helper Functions
-----------------------------------

-- | Typechecks a call expression to ensure the func/proc exists with well-formed arguments 
typecheckCall :: RootTable -> LocalTable -> Ident -> [LocatedExpr]
                           -> Either [AnalysisError] ([TypedExpr], [ProcSymType], Type)
typecheckCall table locals (Ident pos name) args = do
    (params, retType, targetPos) <- case lookupProc table name of
        Just (pos, locals) -> pure (map symType (localParams locals), localRetType locals, Just pos)

        Nothing -> case rawSymType <$> Map.lookup name (localSymbols locals) of
            Just (TFunc params ret) -> pure (params, ret, Nothing)
            _                       -> Left $ errorPos pos $ "unknown procedure " <> backticks name

    -- Check # arguments = # parameters
    unless (length args == length params)
           (let err  = backticks name <> " expects " <> countWithNoun (length params) "parameter"
                                                     <> " but was given " <> tshow (length args)
                note = backticks name <> " declared here:" in
               case targetPos of
                   Just targetPos -> Left $ errorWithNote pos err targetPos note
                   Nothing        -> Left $ errorPos pos err)

    -- Type-check arguments
    typedArgs <- concatEither $ map ((pure <$>) . analyseExpression table locals)
                                    args

    -- Check argument types match parameter types
    let mismatched = filter (\((_, a), b) -> exprType a /= procSymType b)
                            (zip (enumerate typedArgs) params)

    let reportErr ((i, expr), symbol) = Left $ errorPos (locate $ args !! i) $
            "in argument: expecting " <> backticks (procSymType symbol) <> ", found "
                                      <> backticks (exprType expr)
    concatEither $ map reportErr mismatched

    -- Check reference args are filled with lvalues
    let mismatched = filter (\((_, a), b) -> not (checkRefArgs a b))
                            (zip (enumerate typedArgs) params)

    let reportErr ((i, _), _) = Left $ errorPos (locate $ args !! i) "in argument: expecting lvalue"
    concatEither $ map reportErr mismatched

    return (typedArgs, params, retType)
    
    where
        -- | Checks whether the expression and symbol are either:
        --   lvalue + ref
        --   any + val
        checkRefArgs (TypedExpr _ (ELvalue _)) (RefSymbol _) = True
        checkRefArgs _ (RefSymbol _) = False
        checkRefArgs _ _ = True

-- | Type checks a condition to ensure its always bool 
typecheckCondition :: RootTable -> LocalTable -> LocatedExpr -> Either [AnalysisError] Expression 
typecheckCondition symbols locals expr = do 
    TypedExpr ty expr' <- analyseExpression symbols locals expr

    -- condition expression is incorrectly typed 
    if ty /= TBool then 
        Left $ errorPos (locate expr)
                        ("expecting `boolean`, found " <> backticks ty)
    else
        return expr'

-----------------------------------
-- Expression Optimisation 
-----------------------------------

-- | Simplify a given expression by recursively evaluating expressions as much as possible
simplifyExpression :: Expression -> Expression

--   simplify `unaryNot` expression
simplifyExpression (EUnOp UnNot inner)

    = case simplifyExpression (fromLocated inner) of
        EConst (LitBool val) -> EConst (LitBool (not val))
        expr -> EUnOp UnNot (liftExpr expr)

--   simplify `unaryNegation` expression
simplifyExpression (EUnOp UnNegate inner)

    = case simplifyExpression (fromLocated inner) of
        EConst (LitInt val) -> EConst (LitInt (-val))
        expr -> EUnOp UnNegate (liftExpr expr)

--   simplify `and` expression
simplifyExpression (EBinOp BinAnd lhs rhs)

    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        -- As per Piazza, eliminate below short-circuiting.
            
        -- (EConst (LitBool False), _) -> EConst (LitBool False)
        -- (_ , EConst (LitBool False)) -> EConst (LitBool False)
        (EConst (LitBool lhs), EConst (LitBool rhs)) -> EConst (LitBool (lhs && rhs))
        (lhs, rhs) -> EBinOp BinAnd (liftExpr lhs) (liftExpr rhs)

--   simplify `or` expression
simplifyExpression (EBinOp BinOr lhs rhs)

    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        -- As per Piazza, eliminate below short-circuiting.
            
        -- (EConst (LitBool True), _) -> EConst (LitBool True)
        -- (_ , EConst (LitBool True)) -> EConst (LitBool True)
        (EConst (LitBool lhs), EConst (LitBool rhs)) -> EConst (LitBool (lhs || rhs))
        (lhs, rhs) -> EBinOp BinOr (liftExpr lhs) (liftExpr rhs)

--   simplify `plus` expression
simplifyExpression (EBinOp BinPlus lhs rhs)

    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        (EConst (LitInt lhs),  EConst (LitInt rhs))  -> EConst (LitInt (lhs + rhs))
        (lhs, rhs) -> EBinOp BinPlus (liftExpr lhs) (liftExpr rhs)

--   simplify `minus` expression
simplifyExpression (EBinOp BinMinus lhs rhs)

    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        (EConst (LitInt lhs),  EConst (LitInt rhs))  -> EConst (LitInt (lhs - rhs))
        (lhs, rhs) -> EBinOp BinMinus (liftExpr lhs) (liftExpr rhs)

--   simplify `times` expression
simplifyExpression (EBinOp BinTimes lhs rhs)

    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        (EConst (LitInt lhs),  EConst (LitInt rhs))  -> EConst (LitInt (lhs * rhs))
        (lhs, rhs) -> EBinOp BinTimes (liftExpr lhs) (liftExpr rhs)

--   simplify `divide` expression
simplifyExpression (EBinOp BinDivide lhs rhs)

    = case (simplifyExpression (fromLocated lhs), simplifyExpression (fromLocated rhs)) of
        expr@(EConst (LitInt lhs),  EConst (LitInt rhs))  -> 
            if rhs /= 0 then
                EConst (LitInt (lhs `div` rhs))
            else
                uncurry (EBinOp BinDivide) (mapPair liftExpr expr)
        (lhs, rhs) -> EBinOp BinDivide (liftExpr lhs) (liftExpr rhs)

--   The only remaining cases are comparison operators
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

--   No simplication can be done 
simplifyExpression expr = expr

-----------------------------------
-- Lvalue Analysis 
-----------------------------------

-- | A TypedLvalue tracks the following information:
--     * pass-by-ref/value mode 
--     * expected type of the lvalue expression
--     * its root stackslot position
--     * an expression that may/may not evalute to the offset from the root slot 
--     * the name 
--     * source position of the lvalue in the source file 
data TypedLvalue = TypedRefLvalue Type StackSlot Expression Text SourcePos
                 | TypedValLvalue Type StackSlot Expression Text SourcePos

-- | Visualising lvalue types
instance Show TypedLvalue where
    show lval = show (lvalueName lval) <> " (" <> show (lvalueType lval) <> ")"

-- | Analyse a given Lvalue expression and identify if it is semantically valid 
analyseLvalue :: RootTable -> LocalTable -> Lvalue -> Either [AnalysisError] TypedLvalue

--   Analysing the form * <identifier> 
analyseLvalue _ locals (LId (Ident pos name)) = do

    sym <- unwrapOr (Map.lookup name $ localSymbols locals)
                    (Left $ errorPos pos $ "in statement: unknown variable " <> backticks name)
    return $ symToTypedLvalue sym noOffset

--   Analysing the form * <identifier> [<expression>]
analyseLvalue table locals (LArray (Ident pos ident) indexExpr)

    = case Map.lookup ident (localSymbols locals) of
        Just sym -> do
            case procSymType $ symType sym of
                
                -- Make sure the lvalue is an array 
                TArray _ _ ty -> do

                    typecheckArrayIndex table locals indexExpr
                    let pos = locate indexExpr

                    -- Identify the evaluated `index` 
                    index <- 
                        if sizeof ty /= 1 then
                            return $ EBinOp BinTimes
                                        (LocatedExpr pos (EConst (LitInt (sizeof ty))))
                                        indexExpr
                        else
                            return $ simplifyExpression (fromLocated indexExpr)

                    -- Create TypedLvalue that tracks info about the array expression
                    return $ symToTypedLvalue
                        (ProcSymbol (cons sym ty) (symLocation sym) pos (ident <> "[]"))
                        index

                TNever -> Left []

                ty -> Left $ errorPos pos $ "expected array type, found " <> backticks ty

        Nothing  -> Left $ errorPos pos $ "in array expression: unknown variable " <> backticks ident

    where
        cons ty = case symType ty of
            ValSymbol _ -> ValSymbol
            RefSymbol _ -> RefSymbol

--   Analysing the form * <identifier>.<identifier>
analyseLvalue _ locals (LMember (Ident recPos recName) (Ident fldPos fldName)) = do

    recSym <- unwrapOr (Map.lookup recName $ localSymbols locals)
                       (Left $ errorPos recPos $ "in statement: unknown variable "
                                               <> backticks recName)

    let ty = rawSymType recSym

    case ty of

        -- Make sure we're dealing with a record type for member access 
        TRecord _ fieldMap -> do
            
            -- get the symbol of the field member in record 
            fieldSym <- unwrapOr (Map.lookup fldName fieldMap)
                                 (Left $ errorWithNote
                                    fldPos ("in statement: unknown field " <> backticks recName)
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

        TNever -> Left []

        _ -> Left $ errorPos recPos $ "expected variable of record type, found " <> backticks ty

--   Analysing the form * <identifier>[<expression>].<identifier>
analyseLvalue symbols locals (LArrayMember (Ident arrPos arrName) indexExpr (Ident fldPos fldName)) 

    = case Map.lookup arrName (localSymbols locals) of
        Just sym -> do
            case procSymType $ symType sym of

                -- Make sure we're dealing with an array of records 
                TArray _ _ ty@(TRecord _ fields) -> do

                    typecheckArrayIndex symbols locals indexExpr

                    case Map.lookup fldName fields of

                        -- Make sure we're trying to access a field member 
                        Just (Field _ offset innerTy) -> do

                            let pos = locate indexExpr

                            -- Identify the evaluated `index` 
                            index <- 
                                if sizeof ty /= 1 then
                                    return $ EBinOp BinTimes
                                                (LocatedExpr pos (EConst (LitInt (sizeof ty))))
                                                indexExpr
                                else
                                    return $ simplifyExpression (fromLocated indexExpr)
                            
                            -- Create TypedLvalue that tracks info about the array expression
                            return $ symToTypedLvalue
                                (ProcSymbol (cons sym innerTy)
                                            (symLocation sym)
                                            pos
                                            (arrName <> "[]" <> fldName))
                                (EBinOp BinPlus
                                        (LocatedExpr pos ((EConst . LitInt . stackSlotToInt) offset))
                                        (LocatedExpr pos index))
                                        
                        _ -> Left $ errorPos fldPos $ "in expression: unknown field name "
                                                   <> backticks fldName

                TNever -> Left []

                ty -> Left $ errorPos arrPos $ "expected array of records, found " <> backticks ty
        Nothing  -> Left $ errorPos arrPos $ "in array record expression: unknown variable "
                                          <> backticks arrName
    where
        cons ty = case symType ty of
            ValSymbol _ -> ValSymbol
            RefSymbol _ -> RefSymbol

-----------------------------------
-- Lvalue Analysis Helper Functions 
-----------------------------------

-- | Extracting the type from our TypedLvalue
lvalueType :: TypedLvalue -> Type
lvalueType (TypedRefLvalue ty _ _ _ _) = ty
lvalueType (TypedValLvalue ty _ _ _ _) = ty

-- | Extracting the root stack slot location from our TypedLvalue
lvalueLocation :: TypedLvalue -> StackSlot
lvalueLocation (TypedRefLvalue _ loc _ _ _) = loc
lvalueLocation (TypedValLvalue _ loc _ _ _) = loc

-- | Extracting the expression offset from our TypedLvalue
lvalueOffset :: TypedLvalue -> Expression
lvalueOffset (TypedRefLvalue _ _ off _ _) = off
lvalueOffset (TypedValLvalue _ _ off _ _) = off

-- | Extracting the name from our TypedLvalue
lvalueName :: TypedLvalue -> Text
lvalueName (TypedRefLvalue _ _ _ name _) = name
lvalueName (TypedValLvalue _ _ _ name _) = name

-- | Extracting the Source position from out TypedLvalue
lvaluePos :: TypedLvalue -> SourcePos
lvaluePos (TypedRefLvalue _ _ _ _ pos) = pos
lvaluePos (TypedValLvalue _ _ _ _ pos) = pos

-- | Converting a ProcSymbol into a TypedLvalue by attaching its `mode` to an Lvalue Expression
symToTypedLvalue :: ProcSymbol -> Expression -> TypedLvalue
symToTypedLvalue (ProcSymbol (RefSymbol ty) slot pos name) offset
    = TypedRefLvalue ty slot offset name pos

symToTypedLvalue (ProcSymbol (ValSymbol ty) slot pos name) offset
    = TypedValLvalue ty slot offset name pos

-- | An expression representation for there being no offset 
noOffset :: Expression
noOffset = EConst (LitInt 0)

-----------------------------------
-- Validation Functions
-----------------------------------

-- | Checks if the symbol table contains an appropriate main procedure.
hasMain :: RootTable -> Bool
hasMain symbols = case Map.lookup "main" (rootProcs symbols) of
    Just (_, prc) -> null (localParams prc)
    Nothing       -> False

-- | Returns True if the statement *always* returns a value.
returnsValue :: Statement -> Bool
returnsValue (SReturn _) = True
returnsValue (SIfElse _ bodyIf bodyElse) = any returnsValue bodyIf && any returnsValue bodyElse
returnsValue _ = False

-----------------------------------
-- While Loop Analysis 
-----------------------------------

-- | Returns all of the lvalues within an expression. Used to detect possible infinite loops.
lvaluesOf :: Expression -> [Lvalue]
lvaluesOf (ELvalue lval) = [lval]
lvaluesOf (EUnOp _ expr) = lvaluesOf (fromLocated expr)
lvaluesOf (EBinOp _ lhs rhs) = lvaluesOf (fromLocated lhs) <> lvaluesOf (fromLocated rhs)
lvaluesOf _ = []

-- | Returns True if the statement possibly modifies the lvalue.
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

