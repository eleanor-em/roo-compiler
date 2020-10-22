{-# LANGUAGE OverloadedStrings #-}

-- | TODO: Restructure BlockState such that it includes the label increment -> every time we normally re initialise the register values
-- we now reset ther egister value to 0. so we do a put or something for the state monad 

module RooCompile where

import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.Text as T

import Common

import RooAnalyse
import RooAst
-- import RooPrettyPrinter
import SymTable
import Oz
import Control.Monad (unless)

-- | effectively now the global state 
data BlockState = BlockState
    { blockSyms :: RootTable
    , blockInstrs :: [Text]
    , blockNextReg :: Int 
    , nextLabel :: Int}

initialBlockState :: RootTable -> BlockState
initialBlockState symbols = BlockState symbols [] 0 0

resetBlockRegs :: EitherState BlockState ()
resetBlockRegs = do
    current <- getEither
    putEither (current { blockNextReg = 0 })

compileProgram :: Program -> Either [AnalysisError] [Text]
compileProgram program@(Program _ _ procs) = do
    let (errs, symbols) = getAllSymbols program

    if not (hasMain symbols) then
        Left (errs <> [AnalysisError 0 0 "main procedure with no parameters missing"])
    else do
        -- Compile all the procedures in our program. 

        -- execEither: "run the state, return errors and final state"
        --  runEither: "run the state, return errors, final state, *and* an extra value"
        let (errs', result) = execEither (mapM_ compileProc procs) (initialBlockState symbols)
        let output = addHeader (blockInstrs result)

        let allErrs = errs <> errs'

        if null allErrs then
            Right (separate output)
        else
            Left allErrs
    where
        separate  = map (<> "\n")
        addHeader = (["call proc_main", "halt"] <>)

compileProc :: Procedure -> EitherState BlockState ()
compileProc (Procedure _ (ProcHeader (Ident _ procName) _) _ statements) = do
    current <- getEither

    case lookupProc (blockSyms current) procName of
        Just (_, locals) -> do
            let resetAndCompile st = do
                resetBlockRegs
                compileStatement locals st

            case runEither (mapM resetAndCompile statements) current of
                Right (instrs, _) -> do
                    -- handle errs and results
                    let numLocals = Map.size (localSymbols locals)
                    
                    let prologue = if numLocals > 0 then ozPushStackFrame numLocals else []
                    let epilogue = if numLocals > 0 then ozPopStackFrame  numLocals else []

                    -- Load arguments
                    let argPrologue = mconcat $ zipWith ozStore (map symLocation (localParams locals))
                                                                (map Register [0..])

                    let allInstrs = concat
                            [ ["\n" <> makeProcLabel procName <> ":"]
                            , addComment "prologue"
                            , map addIndent (prologue <> argPrologue <> concat instrs)
                            , addComment "epilogue"
                            , map addIndent (epilogue <> ["return"]) ]

                    putEither (current { blockInstrs = allInstrs })

                Left errs -> addErrors errs

        Nothing  -> error "internal error: missed a procedure somehow"


-- get the symbols from root table 
compileStatement :: LocalTable -> Statement -> EitherState BlockState [Text]
-- | write str;
--   Special case to handle string literals.
compileStatement _ (SWrite (LocatedExpr _ (EConst (LitString str))))
    = pure (ozWriteString str)

-- | write expr;
compileStatement locals (SWrite expr) = do
    current <- getEither
    let symbols = rootAliases (blockSyms current)

    let result = do
        TypedExpr ty expr <- analyseExpression symbols locals expr
        (result, final) <- runEither (compileExpr locals expr) current

        let op TInt  = ozWriteInt
            op TBool = ozWriteBool
            op _     = error $ "internal error: attempted to write invalid type `" <> show ty <> "`" in 
            
            return $ blockInstrs final <> op ty result

    case result of
        Left errs -> do
            addErrors errs
            return []
        Right val -> return val

-- | writeln expr;
compileStatement locals (SWriteLn expr) = do
    instrs <- compileStatement locals (SWrite expr)
    return $ instrs <> ozWriteString "\\n"

-- | lvalue <- expr;
compileStatement locals (SAssign lvalue expr) = do
    current <- getEither
    let symbols = rootAliases (blockSyms current)

    let result = do
        TypedExpr ty' expr' <-  analyseExpression symbols locals expr
    
        sym <- analyseLvalue locals lvalue
        let ty = rawSymType sym
        
        if ty /= ty' then
            let err  = "expecting `" <> tshow ty' <> "` on RHS of `<-`, found `" <> tshow ty <> "`"
                note = "`" <> symName sym <> "` declared here:" in
            Left $ errorWithNote (locate expr) err (symPos sym) note
        else do
            (register, postEval) <- runEither (compileExpr locals expr') current
            (_, final)           <- runEither (storeSymbol register sym) postEval
            return $ blockInstrs final

    case result of
        Left errs -> do
            addErrors errs
            return []
        Right val -> return val

compileStatement locals (SRead lvalue) = do
    let result = do
        sym <- analyseLvalue locals lvalue

        -- Handle the different types of symbols appropriately
        case symType sym of
            ValSymbol TInt  -> return $ ozReadInt (symLocation sym)
            ValSymbol TBool -> return $ ozReadBool (symLocation sym)

            RefSymbol TInt  -> return $ ozReadIntIndirect (symLocation sym)
            RefSymbol TBool -> return $ ozReadBoolIndirect (symLocation sym)

            _ -> let err  = "expecting `integer` or `boolean`, found `" <> tshow (rawSymType sym) <> "`"
                     note = "`" <> symName sym <> "` declared here:" in
                Left $ errorWithNote (locateLvalue lvalue) err (symPos sym) note

    case result of
        Left errs -> do
            addErrors errs
            return []
        Right val -> return val
            

compileStatement locals (SCall (Ident pos procName) args) = do
    current <- getEither
    let symbols = blockSyms current
    
    let result = do
        (targetPos, targetProc) <- unwrapOr (Map.lookup procName $ rootProcs symbols)
                                            (liftOne $ errorPos pos $
                                                "unknown procedure `" <> procName <> "`")
        let params = localParams targetProc

        -- Check # arguments = # parameters
        unless  (length args == length params)
                (let err  = mconcat
                        [ "`", procName, "` expects ", countWithNoun (length params) "parameter"
                        , " but was given ", tshow (length args) ]
                     note = "`" <> procName <> "` declared here:" in
                        Left $ errorWithNote pos err targetPos note)

        -- Type-check arguments
        typedArgs <- concatEither $ map ((pure <$>) . analyseExpression (rootAliases symbols) locals)
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

        -- Compile arguments
        let compileArgs = mapM (compileExpr locals . innerExp) typedArgs
        -- Reserve registers for the arguments
        let current = current { blockNextReg = length args }

        (registers, final) <- runEither compileArgs current
        -- TODO: load address as necessary

        let moves = concatMap (uncurry ozMove) (zip (map Register [0..]) registers)

        return $ blockInstrs final <> moves <> ozCall (makeProcLabel procName)
    
    case result of
        Left errs -> do
            addErrors errs
            return []
        Right val -> return val

-- dealing with if statements  
-- compileStatement locals (SIf expr statements) = do 
--     current <- getEither
--     let symbols = rootAliases (blockSyms current)
--     thenLabel <- getLabel 
--     fiLabel <- getLabel 

--     let result = do 
--         TypedExpr ty expr' <- analyseExpression symbols locals expr
    
--         -- condition expression is incorrectly typed 
--         if ty /= TBool then 
--             let err  = "expecting `" <> tshow TBool <> "`, found `" <> tshow ty <> "`" 
--                 note = "expression found here:" in
--             Left $ errorWithNote (locate expr) err (locate expr) note 
--         else do 
            
--             -- get the register where true/false is stored + the current state after compilation
--             (register, final) <- runEither (compileExpr locals expr') current

--             return $ blockInstrs final <> ozBranchOnFalse register fiLabel <> [thenLabel] 
            
        -- addInstrs [label_current]
        -- otherwise do a mapping of compileStatement over [statement]
        -- addInstrs [label_next] 
        -- then return any errors if found or the compiled stuff 
    
    -- case result of
    --     Left errs -> do
    --         addErrors errs
    --         return []
    --     Right val -> return val
    

compileStatement _ _ = error "compileStatement: not yet implemented"

getLabel :: EitherState BlockState Text 
getLabel = do 
    current <- getEither 
    let currentLabel = nextLabel current 
    putEither (current { nextLabel = currentLabel + 1})
    return $ "label_" <> (tshow currentLabel) 

useRegister :: EitherState BlockState Register
useRegister = do
    current <- getEither
    let register = blockNextReg current
    putEither (current { blockNextReg = register + 1})
    return $ Register register

addInstrs :: [Text] -> EitherState BlockState ()
addInstrs instrs = do
    current <- getEither
    let prevInstrs = blockInstrs current
    putEither (current { blockInstrs = prevInstrs <> instrs})

compileExpr :: LocalTable -> Expression -> EitherState BlockState Register
compileExpr locals (ELvalue lvalue) = loadLvalue locals lvalue

compileExpr _ (EConst lit) = loadConst lit

compileExpr locals (EUnOp op expr) = do
    result <- useRegister
    eval <- compileExpr locals (fromLocated expr)
    addInstrs (ozOp result eval)
    return result

    where
        ozOp = case op of
            UnNot    -> ozNot
            UnNegate -> ozNeg

compileExpr locals (EBinOp op lexp rexp) = do
    result <- useRegister
    lhs <- compileExpr locals (fromLocated lexp)
    rhs <- compileExpr locals (fromLocated rexp)
    addInstrs (ozOp result lhs rhs)
    return result

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

loadLvalue :: LocalTable -> Lvalue -> EitherState BlockState Register
loadLvalue locals (LId (Ident _ name)) = do
    case Map.lookup name (localSymbols locals) of
        Just sym -> loadSymbol sym
        Nothing  -> error "internal error: type check failed"

loadLvalue _ _ = error "loadLvalue: not yet implemented"

loadSymbol :: ProcSymbol -> EitherState BlockState Register
loadSymbol (ProcSymbol (ValSymbol _) location _ _) = do
    register <- useRegister
    addInstrs $ ozLoad register location
    return register

loadSymbol (ProcSymbol (RefSymbol _) location _ _) = do
    ptr <- useRegister
    register <- useRegister
    addInstrs $ ozLoad         ptr location
             <> ozLoadIndirect register ptr
    return register

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

storeLvalue :: LocalTable -> Lvalue -> Register -> EitherState BlockState ()
storeLvalue locals lvalue register = do
    addErrorsOr (analyseLvalue locals lvalue) $ \sym ->
        storeSymbol register sym

storeSymbol :: Register -> ProcSymbol -> EitherState BlockState ()
storeSymbol register (ProcSymbol (ValSymbol _) location _ _)
    = addInstrs $ ozStore location register

storeSymbol register (ProcSymbol (RefSymbol _) location _ _) = do
    ptr <- useRegister
    addInstrs $ ozLoad          ptr location
             <> ozStoreIndirect ptr register

-- Text processing for prettifying generated Oz code
addIndent :: Text -> Text
addIndent "" = ""
addIndent str
    | T.head str == '#' = str
    | otherwise         = "    " <> str

addComment :: Text -> [Text]
addComment str = ["# " <> T.strip str]

addCommentTo :: Text -> [Text] -> [Text]
addCommentTo str = (["# " <> T.strip str] <>)

makeProcLabel :: Text -> Text
makeProcLabel = ("proc_" <>)