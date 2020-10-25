{-# LANGUAGE OverloadedStrings #-}

module RooCompile where

import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import Common

import RooAnalyse
import RooAst
import SymTable
import Oz
import Control.Monad (when)
import RooPrettyPrinter (prettyStatement, prettyExpr)

-- | effectively now the global state 
data BlockState = BlockState
    { blockSyms :: RootTable
    , blockInstrs :: [Text]
    , blockNextReg :: Int 
    , blockStackReg :: Int
    , nextLabel :: Int}

initialBlockState :: RootTable -> BlockState
initialBlockState symbols = BlockState symbols [] 0 0 0

resetBlockRegs :: EitherState BlockState ()
resetBlockRegs = do
    current <- getEither
    putEither (current { blockNextReg = 0 })

useRegister :: EitherState BlockState Register
useRegister = do
    current <- getEither
    let register = blockNextReg current
    putEither (current { blockNextReg = register + 1})
    return $ Register register

pushRegister :: Register -> EitherState BlockState ()
pushRegister register = do
    current <- getEither
    let head = blockStackReg current + 1
    putEither (current { blockStackReg = head })

    addInstrs $ ozMove (ozExtraRegisters head) register

popRegister :: Register -> EitherState BlockState ()
popRegister register = do
    current <- getEither
    let head = blockStackReg current
    putEither (current { blockStackReg = head - 1 })

    addInstrs $ ozMove register (ozExtraRegisters head)

-- | Add instructions to the state with indentation.
addInstrs :: [Text] -> EitherState BlockState ()
addInstrs instrs = addInstrsRaw (map addIndent instrs)

addInstrsMaybe :: Maybe [Text] -> EitherState BlockState ()
addInstrsMaybe (Just instrs) = addInstrs instrs
addInstrsMaybe _ = pure ()

-- | Add instructions to the state without indentation.
addInstrsRaw :: [Text] -> EitherState BlockState ()
addInstrsRaw instrs = do
    current <- getEither
    let prevInstrs = blockInstrs current
    putEither (current { blockInstrs = prevInstrs <> instrs})

compileProgram :: Program -> ([AnalysisError], [Text])
compileProgram program@(Program _ _ procs) = do
    let (errs, symbols) = getAllSymbols program

    if not (hasMain symbols) then
        (errs <> [AnalysisError 0 0 "main procedure with no parameters missing"], [])
    else do
        -- Compile all the procedures in our program.
        let (errs', result) = execEither (mapM_ compileProc procs) (initialBlockState symbols)
        let output = addHeader (blockInstrs result)

        let allErrs = errs <> errs'

        (allErrs, separate output)
    where
        separate  = map (<> "\n")
        addHeader = (["call proc_main", "halt"] <>)

compileProc :: Procedure -> EitherState BlockState ()
compileProc (Procedure _ (ProcHeader (Ident _ procName) _) _ _ statements) = do
    current <- getEither

    case lookupProc (blockSyms current) procName of
        Just (_, locals) -> do
            let stackSize = localStackSize locals
            let prologue = if stackSize > 0 then ozPushStackFrame stackSize else []
            let epilogue = if stackSize > 0 then ozPopStackFrame  stackSize else []

            -- Load arguments
            let argPrologue = mconcat $ zipWith ozStore (map symLocation (localParams locals))
                                                        (map Register [0..])

            -- Initialise local variables to 0
            let paramCount = length $ localParams locals

            let localPrologue = if stackSize - 1 - paramCount > 0 then
                    ozIntConst (Register 0) 0 <> concatMap ((`ozStore` Register 0) . StackSlot)
                                                        [paramCount..stackSize - 1]
                else
                    []

            addInstrsRaw $ ["\n" <> makeProcLabel procName <> ":"]
                        <> makeComment "prologue"
            addInstrs (prologue <> makeComment "load args" <> argPrologue
                                <> makeComment "init locals" <> localPrologue)

            mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements

            addInstrs (makeComment "epilogue" <> epilogue <> ["return"])

        Nothing  -> error "internal error: missed a procedure somehow"

commentStatement :: Statement -> EitherState BlockState ()
commentStatement (SWhile _ _) = error "commentStatement: cannot handle `while`"
commentStatement SIfElse {} = error "commentStatement: cannot handle `if...else`"
commentStatement st = addInstrs $ makeComment $ prettyStatement 0 st

-- | Allows us to correctly comment `write` and `writeln` statements
--   Special case to handle string literals.
compileWrite :: LocalTable -> LocatedExpr -> EitherState BlockState ()
compileWrite _ (LocatedExpr _ (EConst (LitString str)))
    = addInstrs (ozWriteString str)

compileWrite locals expr = do    
    current <- getEither
    let symbols = blockSyms current

    addErrorsOr (analyseExpression symbols locals expr) $ \(TypedExpr ty expr) -> do
        register <- compileExpr locals (simplifyExpression expr)

        let op TInt  = addInstrs . ozWriteInt
            op TBool = addInstrs . ozWriteBool
            op _     = error $ "internal error: attempted to write invalid type `" <> show ty <> "`"
            in op ty <?> register

compileCall :: LocalTable -> Ident -> [LocatedExpr] -> EitherState BlockState (Maybe Register)
compileCall locals ident args = do
    current <- getEither
    let symbols = blockSyms current

    case result current symbols of
        Left errs    -> do
            addErrors errs
            return Nothing

        Right (instrs, retType) -> do
            addInstrsRaw instrs
            return $ case retType of
                TVoid -> Nothing
                _     -> Just ozReturnRegister
    
    where
        result current symbols = do
            (typedArgs, params, retType) <- typecheckCall symbols locals ident args
            -- Compile arguments
            let typedArgs' = zip typedArgs params
            let currentReg = blockNextReg current
            let next = current { blockInstrs = [], blockNextReg = currentReg + length args }
            (registers, final) <- runEither (mapM compileArg typedArgs') next

            let moves = concatMap (uncurry ozMove)
                                  (zip (map Register [0..]) (catMaybes registers))

            return (blockInstrs final <> map addIndent (moves <> ozCall (makeProcLabel (fromIdent ident))), retType)

            where
                -- | Compile the argument, loading the address for ref types and the value for val types
                compileArg (TypedExpr _ (ELvalue lvalue), ProcSymbol (RefSymbol _) _ _ _)
                    = loadAddress locals lvalue
                compileArg (TypedExpr _ expr, _)
                    = compileExpr locals (simplifyExpression expr)

compileStatement :: LocalTable -> Statement -> EitherState BlockState ()
-- | write expr;
compileStatement locals st@(SWrite expr) = do
    commentStatement st
    compileWrite locals expr

-- | writeln expr;
compileStatement locals st@(SWriteLn expr) = do
    commentStatement st

    compileWrite locals expr
    addInstrs (ozWriteString "\\n")

-- | lvalue <- expr;
compileStatement locals st@(SAssign lvalue expr) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current

    let analysed = do
        TypedExpr ty' expr' <- analyseExpression symbols locals expr
        sym <- analyseLvalue symbols locals lvalue
        return (ty', expr', sym)

    addErrorsOr analysed $ \(ty', expr', sym) -> do
        let ty = lvalueType sym
        if ty /= ty' then
            addErrors $ errorPos (locate expr)
                                 ("cannot assign `" <> tshow ty' <> "` to `" <> tshow ty <> "`")
        else
            case expr' of
                ELvalue lvalue' -> do
                    addErrorsOr (analyseLvalue symbols locals lvalue')
                                (\sym' -> copyContents sym sym' (sizeof ty'))
                _ -> do
                    register <- compileExpr locals (simplifyExpression expr')
                    storeSymbol locals sym <?> register            

compileStatement locals st@(SRead lvalue) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current
    -- reserve a register for reading the value
    _ <- useRegister

    addErrorsOr (analyseLvalue symbols locals lvalue) $ \sym -> do
        let ty = lvalueType sym

        case ty of
            TInt  -> addInstrs ozReadInt
            TBool -> addInstrs ozReadBool
            _     -> do
                -- TODO: error with note
                let err  = "expecting `integer` or `boolean` after `read`, found `" <> tshow ty <> "`" in
                    addErrors $ errorPos (lvaluePos sym) err

        storeLvalue locals lvalue (Register 0)

compileStatement locals st@(SCall ident args) = do
    commentStatement st
    compileCall locals ident args
    pure ()

-- dealing with if statements  
compileStatement locals (SIf expr statements) = do 
    addInstrs $ makeComment ("if " <> prettyExpr (fromLocated expr) <> " then")

    current <- getEither
    let symbols = blockSyms current
    fiLabel <- getLabel 

    addErrorsOr (analyse symbols) $ \expr' -> do 
        case simplifyExpression expr' of
            EConst (LitBool val) -> addErrors $ warnPos (locate expr)
                                                        ("`if` condition is always " <> tshowBool val)
            _ -> pure ()

        -- get the register where true/false is stored + the current state after compilation
        register <- compileExpr locals (simplifyExpression expr')
        addInstrs (ozBranchOnFalse (fromJust register) fiLabel) 
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements 
        addInstrs $ makeComment "fi"
        addInstrsRaw [fiLabel <> ":"]
    where
        analyse symbols = do 
            TypedExpr ty expr' <- analyseExpression symbols locals expr

            -- condition expression is incorrectly typed 
            if ty /= TBool then 
                Left $ errorPos (locate expr)
                                ("expecting `boolean`, found `" <> tshow ty <> "`")
            else
                return expr'

-- dealing with if/Else statements  
compileStatement locals (SIfElse expr ifStatements elseStatements) = do 
    addInstrs $ makeComment ("if " <> prettyExpr (fromLocated expr) <> " then")

    current <- getEither
    let symbols = blockSyms current
    elseLabel <- getLabel 
    afterLabel <- getLabel

    addErrorsOr (analyse symbols) $ \expr' -> do 
        case simplifyExpression expr' of
            EConst (LitBool val) -> addErrors $ warnPos (locate expr)
                                                        ("`if` condition is always " <> tshowBool val)
            _ -> pure ()

        -- get the register where true/false is stored + the current state after compilation
        register <- compileExpr locals (simplifyExpression expr')
        -- if condition is false -> go to else 
        addInstrs (ozBranchOnFalse (fromJust register) elseLabel) 
        -- otherwise do these statements 
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) ifStatements 
        -- now goto after the if block 
        addInstrs (ozBranch afterLabel)
        addInstrs $ makeComment "else"
        addInstrsRaw [elseLabel <> ":"]
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) elseStatements
        addInstrs $ makeComment "fi"
        addInstrsRaw [afterLabel <> ":"]
    where
        analyse symbols = do 
            TypedExpr ty expr' <- analyseExpression symbols locals expr

            -- condition expression is incorrectly typed 
            if ty /= TBool then 
                Left $ errorPos (locate expr)
                                ("expecting `boolean`, found `" <> tshow ty <> "`")
            else
                return expr'

-- dealing with while statements  
compileStatement locals (SWhile expr statements) = do 
    addInstrs $ makeComment ("while " <> prettyExpr (fromLocated expr) <> " do")

    current <- getEither
    let symbols = blockSyms current
    beginLabel <- getLabel
    falseLabel <- getLabel 

    addErrorsOr (analyse symbols) $ \expr' -> do 
        case simplifyExpression expr' of
            EConst (LitBool val) -> addErrors $ warnPos (locate expr)
                                                        ("`while` condition is always " <> tshowBool val)
            _ -> pure ()

        let conditionLvals = lvaluesOf (fromLocated expr)
        let nonModifiedLvals = filter (\lval -> not (any (modifiesLvalue lval) statements)) conditionLvals

        when (length nonModifiedLvals == length conditionLvals)
             (addErrors $ warnPos
                (locate expr)
                "possible infinite loop: the condition does not change between iterations")

        addInstrsRaw [beginLabel <> ":"]
        -- get the register where true/false is stored + the current state after compilation
        register <- compileExpr locals (simplifyExpression expr')
        -- if condition is false --> skip the while loop 
        addInstrs (ozBranchOnFalse (fromJust register) falseLabel) 
        -- otherwise do these statements
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements 
        -- go to start 
        addInstrs (ozBranch beginLabel)
        addInstrs $ makeComment "od"
        addInstrsRaw [falseLabel <> ":"]
        
    where
        analyse symbols = do 
            TypedExpr ty expr' <- analyseExpression symbols locals expr

            -- condition expression is incorrectly typed 
            if ty /= TBool then 
                Left $ errorPos (locate expr)
                                ("expecting `boolean`, found `" <> tshow ty <> "`")
            else
                return expr'

compileStatement locals st@(SReturn expr) = do
    commentStatement st

    current <- getEither
    let symbols = blockSyms current

    addErrorsOr (analyseExpression symbols locals expr) $ \(TypedExpr ty expr') -> do
        let ty' = localRetType locals

        if ty /= ty' then
            addErrors $ errorPos (locate expr)
                                 ("expecting `" <> tshow ty' <> "` on RHS of `return`, found `" <> tshow ty <> "`")
        else do
            register <- compileExpr locals (simplifyExpression expr')
            (addInstrs .ozMove ozReturnRegister) <?> register

getLabel :: EitherState BlockState Text 
getLabel = do 
    current <- getEither 
    let currentLabel = nextLabel current 
    putEither (current { nextLabel = currentLabel + 1})
    return $ "label_" <> (tshow currentLabel)

compileExpr :: LocalTable -> Expression -> EitherState BlockState (Maybe Register)
compileExpr locals (ELvalue lvalue) = loadLvalue locals lvalue

compileExpr _ (EConst lit) = Just <$> loadConst lit

compileExpr locals (EUnOp op expr) = do
    eval <- compileExpr locals (fromLocated expr)
    case eval of
        Just eval -> do
            result <- useRegister
            addInstrs (ozOp result eval)
            return $ Just result
        _ -> return Nothing

    where
        ozOp = case op of
            UnNot    -> ozNot
            UnNegate -> ozNeg

compileExpr locals (EBinOp op lexp rexp) = do
    lhs <- compileExpr locals (fromLocated lexp)
    rhs <- compileExpr locals (fromLocated rexp)
    case (lhs, rhs) of
        (Just lhs, Just rhs) -> do
            result <- useRegister
            addInstrs (ozOp result lhs rhs)
            return $ Just result
        _ -> return Nothing

    where
        ozOp = case op of
            BinOr    -> ozOr
            BinAnd   -> ozAnd
            BinEq    -> ozEq
            BinNeq   -> ozNeq
            BinLt    -> ozLt
            BinLte   -> ozLte
            BinGt    -> ozGt
            BinGte   -> ozGte
            BinPlus  -> ozPlus
            BinMinus -> ozMinus
            BinTimes -> ozTimes
            BinDivide -> ozDivide

compileExpr locals (EFunc func args) = do
    current <- getEither
    let usedRegisters = blockNextReg current - 1

    when (usedRegisters >= 0) (do
        -- Save the current registers
        addInstrs $ makeComment "push registers"
        mapM_ (pushRegister . Register) [0..usedRegisters])

    addInstrs $ makeComment $ "call " <> fromIdent func
    result <- useRegister
    returned <- compileCall locals func args
    addInstrs $ ozMove result ozReturnRegister

    when (usedRegisters >= 0) (do
        -- Save the current registers
        addInstrs $ makeComment "pop registers"
        mapM_ (popRegister . Register) [0..usedRegisters])
    
    case returned of
        Just _   -> return $ Just result
        _        -> return Nothing

loadLvalue :: LocalTable -> Lvalue -> EitherState BlockState (Maybe Register)
loadLvalue locals lvalue = do
    current <- getEither

    case analyseLvalue (blockSyms current) locals lvalue of
        Left errs -> do
            addErrors errs
            return Nothing
        Right sym -> loadSymbol locals sym


loadAddress :: LocalTable -> Lvalue -> EitherState BlockState (Maybe Register)
loadAddress locals lvalue = do
    current <- getEither

    case analyseLvalue (blockSyms current) locals lvalue of
        Left errs -> do
            addErrors errs
            return Nothing

        Right lval -> do
            let offset = lvalueOffset lval
            let location = lvalueLocation lval

            if noOffset == offset then do
                -- Load the address directly
                register <- useRegister
                addInstrs $ loadOp lval register location
                return $ Just register
            else do
                -- Load the address and add offset
                baseReg <- useRegister
                offsetReg <- compileExpr locals offset
                case offsetReg of
                    Just offsetReg -> do
                        addInstrs $ loadOp lval baseReg location
                                 <> ozSubOffset baseReg baseReg offsetReg
                        return $ Just baseReg
                    _ -> return Nothing
    where
        loadOp lval = case lval of
            TypedValLvalue {} -> ozLoadAddress
            TypedRefLvalue {} -> ozLoad

loadSymbol :: LocalTable -> TypedLvalue -> EitherState BlockState (Maybe Register)
loadSymbol locals lval = do
    let offset = lvalueOffset lval
    let location = lvalueLocation lval

    if noOffset == offset then do
        register <- useRegister
        noOffsetOp register
        return $ Just register
    else do
        -- Compute a reference offset
        register <- useRegister
        baseReg <- useRegister
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> do
                addInstrs $ offsetLoad baseReg location
                         <> ozSubOffset baseReg baseReg offsetReg
                         <> ozLoadIndirect register baseReg
                return $ Just register
            _ -> return Nothing
    where
        noOffsetOp register = case lval of
            TypedValLvalue {} ->
                addInstrs $ ozLoad register (lvalueLocation lval) 

            TypedRefLvalue {} -> do
                ptr <- useRegister
                addInstrs $ ozLoad ptr (lvalueLocation lval)
                         <> ozLoadIndirect register ptr
        
        offsetLoad = case lval of
            TypedValLvalue {} -> ozLoadAddress
            TypedRefLvalue {} -> ozLoad

loadConst :: Literal -> EitherState BlockState Register
loadConst (LitBool val) = do
    register <- useRegister
    addInstrs $ ozBoolConst register val
    return register

loadConst (LitInt val) = do
    register <- useRegister
    addInstrs $ ozIntConst register val
    return register

loadConst _ = error "internal error: tried to load Text constant"

-- HMMMM incomplete
loadSlot :: TypedLvalue -> Register -> EitherState BlockState Register 
loadSlot lval offsetReg = do
    register <- useRegister 
    baseReg <- useRegister 
    addInstrs $ offsetLoad baseReg (lvalueLocation lval) 
            <> ozSubOffset baseReg baseReg offsetReg
            <> ozLoadIndirect register baseReg 
    return register 

    where 
        offsetLoad = case lval of 
            TypedValLvalue {} -> ozLoadAddress 
            TypedRefLvalue {} -> ozLoad

storeContent :: TypedLvalue -> Register -> Register -> EitherState BlockState ()
storeContent lval offsetReg fromRegister = do
    baseReg <- useRegister 
    addInstrs $ offsetLoad baseReg (lvalueLocation lval) 
            <> ozSubOffset baseReg baseReg offsetReg
            <> ozStoreIndirect baseReg fromRegister 

    where 
        offsetLoad = case lval of 
            TypedValLvalue {} -> ozLoadAddress 
            TypedRefLvalue {} -> ozLoad

copyContents :: TypedLvalue -> TypedLvalue -> Int -> EitherState BlockState () 
copyContents lval rval size = do
    offsetReg <- loadConst (LitInt 0)
    copyContentsRec lval rval offsetReg size
    
copyContentsRec :: TypedLvalue -> TypedLvalue -> Register -> Int -> EitherState BlockState ()
copyContentsRec _ _ _ 0 = pure $ ()
copyContentsRec lval rval offsetReg slotsRemaining = do 

    -- load address from rval into a register 
    fromRegister <- loadSlot rval offsetReg 
    -- using the address from before, store content into our lval 
    storeContent lval offsetReg fromRegister
    incrReg <- loadConst (LitInt 1) 
    addInstrs $ ozPlus offsetReg offsetReg incrReg 
    copyContentsRec lval rval offsetReg (slotsRemaining - 1)
    
storeLvalue :: LocalTable -> Lvalue -> Register -> EitherState BlockState ()
storeLvalue locals lvalue register = do
    current <- getEither
    addErrorsOr (analyseLvalue (blockSyms current) locals lvalue)
                (\sym -> storeSymbol locals sym register)

-- TODO: refactor loads as below
storeSymbol :: LocalTable -> TypedLvalue -> Register -> EitherState BlockState ()
storeSymbol locals lval register = do
    let offset = lvalueOffset lval

    if noOffset == offset then
        noOffsetOp
    else do
        baseReg <- useRegister
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> addInstrs $ offsetLoad baseReg (lvalueLocation lval)
                                       <> ozSubOffset baseReg baseReg offsetReg
                                       <> ozStoreIndirect baseReg register
            _ -> return ()
    where
        noOffsetOp = case lval of
            TypedValLvalue {} ->
                addInstrs $ ozStore (lvalueLocation lval) register

            TypedRefLvalue {} -> do
                ptr <- useRegister
                addInstrs $ ozLoad ptr (lvalueLocation lval)
                         <> ozStoreIndirect ptr register
        
        offsetLoad = case lval of
            TypedValLvalue {} -> ozLoadAddress
            TypedRefLvalue {} -> ozLoad

-- Text processing for prettifying generated Oz code
addIndent :: Text -> Text
addIndent "" = ""
addIndent str
    | T.head str == '#' = str
    | otherwise         = "    " <> str

makeComment :: Text -> [Text]
makeComment str = ["# " <> T.strip str]

makeProcLabel :: Text -> Text
makeProcLabel = ("proc_" <>)
