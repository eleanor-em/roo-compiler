{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : RooCompile
Description : Generates the Oz code for a given Roo Source code  

RooCompile takes an AST generated from our parser and generates 
appropriate Oz code. It passed the ast through type checking analysis 
from RooAnalyse and appropriate generates Oz code to handle 
the expected functionality. 
-}

module RooCompile where

import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import Common

import RooAnalyse
import RooAst
import SymTable
import Oz
import RooPrettyPrinter (prettyStatement, prettyExpr)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import RooPreprocessor 

-- | Global state for the compiler. Stores:
--   * the root-level symbol table
--   * the instructions generated so far
--   * the next unused register
--   * the next "stack slot" available for saving registers (uses the end of the register list)
--   * the next unused label index
--   * whether the context is a tail-call
--   * the epilogue for this block (used to ensure returns clean up the stack)
--   * the next unused lambda index
data BlockState = BlockState
    { blockSyms :: RootTable
    , blockInstrs :: [Text]
    , blockNextReg :: Int 
    , blockStackReg :: Int
    , nextLabel :: Int
    , isTailCall :: Bool
    , blockEpilogue :: [Text]
    , blockNextLambda :: Int }

-----------------------------------
-- Pre-Compilation Steps
-----------------------------------

-- | Returns any syntax errors from a program, drawing symbols from a list of includes.
verifyProgram :: Program -> [Program] -> [AnalysisError]
verifyProgram program includes = do
    -- Extract symbols from the includes...
    let (symbols, _, _) = unzip3 $ map (compileSymbols mempty) includes
    -- ...then finish the symbols with the main file
    let (symbols', errs, procs) = compileSymbols (mconcat symbols) program
    fst $ compileWithSymbols symbols' errs procs

-- | Compiles a program fragment; that is, a program that may not have a main procedure.
compileProgramFragment :: Program -> ([AnalysisError], [Text])
compileProgramFragment program = compileWithSymbols symbols errs procs
    where
        (symbols, errs, procs) = compileSymbols mempty program

-- | Compile the program with the given symbol table, previous errors, and procedure list.
compileWithSymbols :: RootTable -> [AnalysisError] -> [Procedure] -> ([AnalysisError], [Text])
compileWithSymbols symbols errs procs = do
    let procs' = map tagProcStatements procs
    -- Compile all the procedures in our program.
    let (errs', result) = execEither (mapM_ compileProc procs')
                                     (initialBlockState symbols)

    let output = addHeader symbols (blockInstrs result)

    let allErrs = errs <> errs'

    (allErrs, separate output)
    where
        separate = map (<> "\n")
        addHeader symbols = ((["call proc_main", "halt"] <> generateVtable symbols) <>)

-- | Compiles the symbols in a given program, with an initial symbol table provided.
compileSymbols :: RootTable -> Program -> (RootTable, [AnalysisError], [Procedure])
compileSymbols initial (Program recs arrs procs) = (symbols, errs, procs')
    where
        -- Count the number of lambda functions in the initial table
        lambdaCount = Map.size $ Map.filterWithKey (\key _ -> "__lambda" `T.isInfixOf` key)
                                                   (rootProcs initial)

        procs' = procs <> concat (evalState (mapM compileLambdas procs) lambdaCount)

        (errs, symbols) = getAllSymbols (Program recs arrs procs') initial

-----------------------------------
-- Main Compilation Functions  
-----------------------------------

-- | Compiles a program.
compileProgram :: Program -> ([AnalysisError], [Text])
compileProgram program = do
    let (symbols, errs, procs) = compileSymbols mempty program
    if not (hasMain symbols) then
        (errs <> [AnalysisError 0 0 "main procedure with no parameters missing"], [])
    else
        compileWithSymbols symbols errs procs

-- | Compiles a procedure definition, updating the state with the instructions and any errors.
compileProc :: Procedure -> EitherState BlockState ()
compileProc (Procedure _ (ProcHeader (Ident pos procName) _) retType _ statements) = do
    current <- getEither

    case lookupProc (blockSyms current) procName of
        Just (_, locals) -> do
            -- Check that non-void procedures always return a value
            unless (retType == VoidTypeName || any returnsValue statements)
                (addErrors $ warnPos pos
                "control may reach the end of the procedure without returning a value")

            let stackSize = localStackSize locals
            let prologue =  if stackSize > 0 then ozPushStackFrame stackSize else []
            let epilogue = (if stackSize > 0 then ozPopStackFrame  stackSize else []) <> ["return"]

            putEither (current { blockEpilogue = epilogue })
            -- Load arguments
            let argPrologue = mconcat $ zipWith ozStore (map symLocation (localParams locals))
                                                        (map Register [0..])

            -- Initialise local variables to 0
            let paramCount = length $ localParams locals

            let localPrologue = if stackSize - paramCount > 0 then
                    makeComment "init locals" <> ozIntConst (Register 0) 0
                                            <> concatMap ((`ozStore` Register 0) . StackSlot)
                                                        [paramCount..stackSize - 1]
                else
                    []

            addInstrsRaw $ ["\n" <> makeProcLabel procName <> ":"]
                        <> makeComment "prologue"
            addInstrs $ prologue <> makeComment "load args" <> argPrologue
            addInstrsRaw [makeProcTailLabel procName <> ":"]
            addInstrs localPrologue

            mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements

            addInstrs (makeComment "epilogue" <> epilogue)

        Nothing  -> error "internal error: missed a procedure somehow"

-----------------------------------
-- Compiling Statements 
-----------------------------------

-- | Compiles a single statement, updating the state with the instructions and any errors.
compileStatement :: LocalTable -> Statement -> EitherState BlockState ()

--   Compiles a `write` statement 
compileStatement locals st@(SWrite expr) = do
    commentStatement st
    compileWrite locals expr

--   Compiles a `writeln` statement
compileStatement locals st@(SWriteLn expr) = do
    commentStatement st
    compileWrite locals expr
    addInstrs (ozWriteString "\\n")

--   Compiles an `lvalue <- expr` statement
compileStatement locals st@(SAssign lvalue expr) = do
    commentStatement st
    current <- getEither
    let symbols = blockSyms current

    -- Extract information from lval/rval 
    let analysed = do
        TypedExpr ty' expr' <- analyseExpression symbols locals expr
        sym <- analyseLvalue symbols locals lvalue
        return (ty', expr', sym)

    -- If no errors occur analysing our lval/rval, we compile 
    addErrorsOr analysed $ \(ty', expr', sym) -> do

        -- Ensure matching types for assignments 
        let ty = lvalueType sym
        if ty /= ty' then
            addErrors $ errorPos (locate expr)
                                 ("cannot assign " <> backticks ty' <> " to " <> backticks ty)
        else
            -- Handle evaluated primitive 
            if isPrimitive ty || isFunction ty then do
                register <- compileExpr locals (simplifyExpression expr')
                storeSymbol locals sym <?> register
            
            -- Handle record/array assignments 
            else case expr' of
                ELvalue lvalue' -> do
                    addErrorsOr (analyseLvalue symbols locals lvalue')
                                (\sym' -> copyContents locals sym sym' (sizeof ty'))
                _ -> error "internal error: somehow had primitive on rhs when expecting lvalue"

--   Compiles a `read` statement 
compileStatement locals st@(SRead lvalue) = do
    commentStatement st
    current <- getEither
    let symbols = blockSyms current

    -- reserve a register for reading the value
    _ <- useRegister

    addErrorsOr (analyseLvalue symbols locals lvalue) $ \sym -> do
        let ty = lvalueType sym

        -- only allow reading primitives 
        case ty of
            TInt  -> addInstrs ozReadInt
            TBool -> addInstrs ozReadBool
            _     -> addErrors $ errorPos (lvaluePos sym) $
                "expecting `integer` or `boolean` on RHS of `read`, found `" <> backticks ty

        storeLvalue locals lvalue (Register 0)

--   Compiles a `call` statement 
compileStatement locals st@(SCall ident args) = do
    commentStatement st
    compileCall locals ident args
    pure ()

--   Compiles a `call` statement that is tail recursive
compileStatement locals st@(STailStatement (SCall ident args)) = do
    commentStatement st
    when (localProcName locals == fromIdent ident) setTailCall
    compileCall locals ident args
    unsetTailCall
    pure ()

--   Compiles any other tail recursive statement (it won't actually do anything different)
compileStatement locals (STailStatement st) = compileStatement locals st

--   Compiles an `if` statement
compileStatement locals (SIf expr statements) = do 
    addInstrs $ makeComment ("if " <> prettyExpr (fromLocated expr) <> " then")
    current <- getEither
    let symbols = blockSyms current
    fiLabel <- getLabel 

    addErrorsOr (typecheckCondition symbols locals expr) $ \expr' -> do 

        -- report trivial cases for conditional expressions
        checkConditionState "if" expr

        register <- compileExpr locals (simplifyExpression expr')        
        addInstrs (ozBranchOnFalse (fromJust register) fiLabel) 

        -- compile potential nested statements  
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements 
        
        addInstrs $ makeComment "fi"
        addInstrsRaw [fiLabel <> ":"]

--   Compiles an `if/Else` statement 
compileStatement locals (SIfElse expr ifStatements elseStatements) = do 
    addInstrs $ makeComment ("if " <> prettyExpr (fromLocated expr) <> " then")
    current <- getEither
    let symbols = blockSyms current
    elseLabel <- getLabel 
    afterLabel <- getLabel

    addErrorsOr (typecheckCondition symbols locals expr) $ \expr' -> do 

        -- report trivial cases for conditional expressions
        checkConditionState "if" expr

        register <- compileExpr locals (simplifyExpression expr')

        -- if condition is false -> go to else 
        addInstrs (ozBranchOnFalse (fromJust register) elseLabel) 

        -- otherwise compile potential nested statements 
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) ifStatements 
        
        -- now goto after the if block 
        addInstrs (ozBranch afterLabel)
        addInstrs $ makeComment "else"
        addInstrsRaw [elseLabel <> ":"]

        -- compile potential nested statements 
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) elseStatements
        
        addInstrs $ makeComment "fi"
        addInstrsRaw [afterLabel <> ":"]

--   Compiles a `while` statement
compileStatement locals (SWhile expr statements) = do 
    addInstrs $ makeComment ("while " <> prettyExpr (fromLocated expr) <> " do")
    current <- getEither
    let symbols = blockSyms current
    beginLabel <- getLabel
    falseLabel <- getLabel 

    addErrorsOr (typecheckCondition symbols locals expr) $ \expr' -> do 
        
        -- report trivial cases for conditional expressions
        checkConditionState "while" expr

        -- Check for trivial cases of possible infinite loops
        let conditionLvals = lvaluesOf (fromLocated expr)
        let nonModifiedLvals = filter (\lval -> not (any (modifiesLvalue lval) statements))
                                      conditionLvals

        when (length nonModifiedLvals == length conditionLvals)
             (addErrors $ warnPos
                (locate expr)
                "possible infinite loop: the condition does not change between iterations")

        addInstrsRaw [beginLabel <> ":"]
        
        register <- compileExpr locals (simplifyExpression expr')

        -- if condition is false --> skip the while loop 
        addInstrs (ozBranchOnFalse (fromJust register) falseLabel) 

        -- otherwise compile potential nested statements 
        mapM_ (\st -> resetBlockRegs >> compileStatement locals st) statements 
        
        -- repeat while loop  
        addInstrs (ozBranch beginLabel)
        addInstrs $ makeComment "od"
        addInstrsRaw [falseLabel <> ":"]

--   Compiles a `return` statement
compileStatement locals st@(SReturn expr) = do
    commentStatement st
    current <- getEither
    let symbols = blockSyms current

    addErrorsOr (analyseExpression symbols locals expr) $ \(TypedExpr ty expr') -> do
        
        let ty' = localRetType locals

        -- Make sure return type is appropriate 
        if ty /= ty' then
            addErrors $ errorPos (locate expr)
                                 ("expecting " <> backticks ty' <> " on RHS of `return`, found "
                                               <> backticks ty)
        else do
            let compile = do
                register <- compileExpr locals (simplifyExpression expr')
                (addInstrs . ozMove ozReturnRegister) <?> register

            -- Tail call optimisation :-)
            case expr' of
                EFunc target _ ->
                    if fromIdent target == localProcName locals then do
                        setTailCall
                        compile
                        unsetTailCall
                    else
                        compile
                _ -> compile

            addInstrs $ blockEpilogue current

-----------------------------------
-- Compiling Expressions 
-----------------------------------

-- | Compiles a single expression, updating the state with the instructions and any errors. 
compileExpr :: LocalTable -> Expression -> EitherState BlockState (Maybe Register)

--   Compiles an `lvalue` expression
compileExpr locals (ELvalue lvalue) = compileLvalue locals lvalue

--   Compiles a `constant`
compileExpr _ (EConst lit) = Just <$> loadConst lit

--   Compiles `unary` expression
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

--   Compiles `binaryOp` expression 
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

--   Compiles a `function` expression 
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

--   Compiles a `lambda` expression 
compileExpr locals (ELambda _ (LocatedTypeName pos _) _ _) = do
    -- Allocate a lambda
    current <- getEither
    let nextLambda = blockNextLambda current
    putEither ( current { blockNextLambda = nextLambda + 1 })

    addInstrs $ makeComment ("call lambda " <> lambdaLabel nextLambda)

    compileExpr locals (ELvalue (LId (Ident pos (lambdaLabel nextLambda))))

-- | Compiling lvalue expressions 
compileLvalue :: LocalTable -> Lvalue -> EitherState BlockState (Maybe Register)
compileLvalue locals lvalue = do
    current <- getEither

    case analyseLvalue (blockSyms current) locals lvalue of
        Left errs -> case lvalue of
            LId (Ident _ name) ->
                case Map.lookup name (rootFuncPtrs $ blockSyms current) of
                    Just (procIndex, _) ->
                        pure <$> loadConst (LitInt procIndex)

                    Nothing -> do 
                        addErrors errs
                        return Nothing
            _ -> do
                addErrors errs
                return Nothing
        Right sym -> loadSymbol locals sym

-----------------------------------
-- Compiling Write Statements 
-----------------------------------

-- | Compile a `write` statement with a given expression, updating the state with the instructions
--   and any errors.
compileWrite :: LocalTable -> LocatedExpr -> EitherState BlockState ()

--   Special case to handle string literals.
compileWrite _ (LocatedExpr _ (EConst (LitString str)))
    = addInstrs (ozWriteString str)

--   Compile a given expression to be written 
compileWrite locals expr = do    
    current <- getEither
    let symbols = blockSyms current

    addErrorsOr (analyseExpression symbols locals expr) $ \(TypedExpr ty expr) -> do
        register <- compileExpr locals (simplifyExpression expr)

        let op TInt  = addInstrs . ozWriteInt
            op TBool = addInstrs . ozWriteBool
            op _     = error $ "internal error: attempted to write invalid type "
                            <> T.unpack (backticks ty)
            in op ty <?> register

-----------------------------------
-- Compiling Call Statements 
-----------------------------------

-- | Compile a procedure call given the procedure's name and its actual arguments.
--   Updates the state with the instructions and any errors.
compileCall :: LocalTable -> Ident -> [LocatedExpr] -> EitherState BlockState (Maybe Register)
compileCall locals ident args = do
    current <- getEither
    let symbols = blockSyms current
    let result = do

        (typedArgs, params, retType) <- typecheckCall symbols locals ident args

        -- Compile arguments
        let typedArgs' = zip typedArgs params
        let currentReg = blockNextReg current
        let next = current { blockInstrs = [], blockNextReg = currentReg + length args }
        (registers, final) <- runEither (mapM compileArg typedArgs') next

        -- handle the case where it's a HOF
        case lookupProc (blockSyms current) (fromIdent ident) of
            Just _ ->
                if isTailCall current then do
                    let stores = concatMap (uncurry ozStore)
                                           (zip (map StackSlot [0..]) (catMaybes registers))

                    let instrs = stores <> ozBranch (makeProcTailLabel (fromIdent ident))
                    let nextLambda = blockNextLambda final
                    return (blockInstrs final <> map addIndent instrs, retType, nextLambda)

                else do
                    let moves = concatMap (uncurry ozMove)
                                          (zip (map Register [0..]) (catMaybes registers))

                    let instrs = moves <> ozCall (makeProcLabel (fromIdent ident))
                    let nextLambda = blockNextLambda final
                    return (blockInstrs final <> map addIndent instrs, retType, nextLambda)
            
            Nothing -> do
                (ptr, final') <- runEither (compileLvalue locals (LId ident)) final

                case ptr of 
                    Just ptr -> do
                        let moves = concatMap (uncurry ozMove)
                                              (zip (map Register [0..]) (catMaybes registers))
                        let instrs = moves <> ozSetVPtr ptr <> ozCall vTableLabel
                        let nextLambda = blockNextLambda final'
                        return (blockInstrs final' <> map addIndent instrs, retType, nextLambda)
                    Nothing -> error "internal error: did not catch all HOF call cases :("

    case result of
        Left errs    -> do
            addErrors errs
            return Nothing

        Right (instrs, retType, nextLambda) -> do
            addInstrsRaw instrs
            current' <- getEither
            putEither (current' { blockNextLambda = nextLambda })
            return $ case retType of
                TVoid -> Nothing
                _     -> Just ozReturnRegister
    where
        -- | Compile the argument, loading the address for refsand the value for vals
        compileArg (TypedExpr _ (ELvalue lvalue), RefSymbol _)
            = loadAddress locals lvalue
        compileArg (TypedExpr _ expr, _)
            = compileExpr locals (simplifyExpression expr)

-----------------------------------
-- Compile Statements Helper Functions 
-----------------------------------

-- | Function for copying the contents of arrays and records in assignments
copyContents :: LocalTable -> TypedLvalue -> TypedLvalue -> Int -> EitherState BlockState () 
copyContents locals lval rval size = do

    -- Calculate the offsets from root slot for both lval/rval
    offsetReg' <- compileExpr locals (lvalueOffset lval)
    offsetReg'' <- compileExpr locals (lvalueOffset rval)

    case (offsetReg', offsetReg'') of 
        (Just offsetReg', Just offsetReg'') -> copyContentsRec lval rval offsetReg' offsetReg'' size 
        _ ->  pure ()

-- | Function that recursively copies the contents of the stack slots until 
--   we've hit the total size of our array/record 
copyContentsRec :: TypedLvalue -> TypedLvalue -> Register -> Register -> Int -> EitherState BlockState ()
copyContentsRec _ _ _ _ 0 = pure $ ()
copyContentsRec lval rval lOffset rOffset slotsRemaining = do 

    -- load address from rval into a register 
    fromRegister <- loadSlot rval rOffset 
    
    -- using the address from before, store content into our lval 
    storeSlot lval lOffset fromRegister

    -- increment our offset registers to move onto the next slots 
    incrReg <- loadConst (LitInt 1) 
    addInstrs $ ozPlus rOffset rOffset incrReg 
    addInstrs $ ozPlus lOffset lOffset incrReg

    copyContentsRec lval rval lOffset rOffset (slotsRemaining - 1)

-- | Check whether the conditional expression is just `true` or `false`
--   and adds and error to our state 
checkConditionState :: Text -> LocatedExpr -> EitherState BlockState ()
checkConditionState st expr = do 
    case simplifyExpression (fromLocated expr) of
        EConst (LitBool val) -> addErrors $
            warnPos (locate expr)
                    ("`" <> st <> "`" <> " condition is always " <> tshowBool val)
        _ -> pure ()

-----------------------------------
-- Generating Loading Lvalues in Oz
-----------------------------------

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

    if noOffset == offset then do
        register <- useRegister
        noOffsetOp register
        return $ Just register
    else do
        -- Compute a reference offset
        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> do
                register <- loadSlot lval offsetReg 
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

-- | Generate Oz code for loading int/bool constants 
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

-- | Load a particular stack stock with offsets 
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

-----------------------------------
-- Generating Storing Lvalues in Oz
-----------------------------------

-- | Store Lvalues in the given register 
storeLvalue :: LocalTable -> Lvalue -> Register -> EitherState BlockState ()
storeLvalue locals lvalue register = do
    current <- getEither
    addErrorsOr (analyseLvalue (blockSyms current) locals lvalue)
                (\sym -> storeSymbol locals sym register)

-- | Store symbols in a given register 
storeSymbol :: LocalTable -> TypedLvalue -> Register -> EitherState BlockState ()
storeSymbol locals lval register = do
    let offset = lvalueOffset lval

    if noOffset == offset then
        noOffsetOp
    else do

        offsetReg <- compileExpr locals offset
        case offsetReg of
            Just offsetReg -> storeSlot lval offsetReg register 
            _ -> return ()
    where
        noOffsetOp = case lval of
            TypedValLvalue {} ->
                addInstrs $ ozStore (lvalueLocation lval) register

            TypedRefLvalue {} -> do
                ptr <- useRegister
                addInstrs $ ozLoad ptr (lvalueLocation lval)
                         <> ozStoreIndirect ptr register

-- | Store the contents of a given lvalue in a stack slot with offset 
storeSlot :: TypedLvalue -> Register -> Register -> EitherState BlockState ()
storeSlot lval offsetReg fromRegister = do
    baseReg <- useRegister 
    
    addInstrs $ offsetLoad baseReg (lvalueLocation lval) 
            <> ozSubOffset baseReg baseReg offsetReg
            <> ozStoreIndirect baseReg fromRegister 

    where 
        offsetLoad = case lval of 
            TypedValLvalue {} -> ozLoadAddress 
            TypedRefLvalue {} -> ozLoad
-----------------------------------
-- Register Management
-----------------------------------

-- | Resets the allocation of registers in the state.
resetBlockRegs :: EitherState BlockState ()
resetBlockRegs = do
    current <- getEither
    putEither (current { blockNextReg = 0 })

-- | Allocates a new register, and returns the register.
useRegister :: EitherState BlockState Register
useRegister = do
    current <- getEither
    let register = blockNextReg current
    if register >= 1024 then
        error "internal error: ran out of registers"
    else do
        putEither (current { blockNextReg = register + 1})
        return $ Register register

-- | Pushes a register to the faux-stack formed by the final registers.
--   Used to save registers between procedure calls on the RHS of statements.
pushRegister :: Register -> EitherState BlockState ()
pushRegister register = do
    current <- getEither
    let head = blockStackReg current + 1
    putEither (current { blockStackReg = head })

    addInstrs $ ozMove (ozExtraRegisters head) register

-- | Pops a register from the faux-stack formed by the final registers.
--   Used to save registers between procedure calls on the RHS of statements.
popRegister :: Register -> EitherState BlockState ()
popRegister register = do
    current <- getEither
    let head = blockStackReg current
    putEither (current { blockStackReg = head - 1 })

    addInstrs $ ozMove register (ozExtraRegisters head)

-----------------------------------
-- General BlockState Management 
-----------------------------------

-- | Creates an empty block state from the given symbol table.
initialBlockState :: RootTable -> BlockState
initialBlockState symbols = BlockState symbols [] 0 0 0 False [] 0

-- | Adds the "this is a tail call" flag to the state.
setTailCall :: EitherState BlockState ()
setTailCall = do
    current <- getEither
    putEither (current { isTailCall = True })

-- | Removes the "this is a tail call" flag from the state.
unsetTailCall :: EitherState BlockState ()
unsetTailCall = do
    current <- getEither
    putEither (current { isTailCall = False })

-- | Add instructions to the state with indentation.
addInstrs :: [Text] -> EitherState BlockState ()
addInstrs instrs = addInstrsRaw (map addIndent instrs)

-- | Add instructions to the state without indentation.
addInstrsRaw :: [Text] -> EitherState BlockState ()
addInstrsRaw instrs = do
    current <- getEither
    let prevInstrs = blockInstrs current
    putEither (current { blockInstrs = prevInstrs <> instrs})

-- | Adds a comment to the state by pretty-printing the statement.
--   Doesn't work nicely for multi-line statements.
commentStatement :: Statement -> EitherState BlockState ()
commentStatement st = addInstrs $ makeComment $ prettyStatement 0 st

-- | Get the next available label from the state and update the counter
-- for next use & format the label as appropriate. 
getLabel :: EitherState BlockState Text 
getLabel = do 
    current <- getEither 
    let currentLabel = nextLabel current 
    putEither (current { nextLabel = currentLabel + 1})
    return $ "label_" <> tshow currentLabel
