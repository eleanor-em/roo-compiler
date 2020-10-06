module SymTable where

import Control.Monad.State

import System.IO.Unsafe (unsafePerformIO)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Text.Pretty.Simple (pPrint)

import Common
import RooAst

-- | A procedure symbol can be either a value or a reference.
data ProcSymType = ValSymbol Type | RefSymbol Type
    deriving Eq

procSymType :: ProcSymType -> Type
procSymType (ValSymbol ty) = ty
procSymType (RefSymbol ty) = ty

instance Show ProcSymType where
    show (ValSymbol ty) = show ty ++ " val"
    show (RefSymbol ty) = show ty ++ " ref"

-- | Represents a procedure symbol with a type, location on the stack, and source position.
data ProcSymbol = ProcSymbol { symType :: ProcSymType, symLocation :: Int, symPos :: SourcePos }
    deriving Eq

instance Show ProcSymbol where
    show sym = concat
        [ show $ symType sym
        , ":"
        , show . sourceLine $ symPos sym
        , ":"
        , show . sourceColumn $ symPos sym
        , " ("
        , show $ symLocation sym
        , ")" ]

-- | Represents a local symbol table for a procedure. Contains the types and ref/val status of
--   parameters in order, and a table of procedure symbols.
data LocalTable = LocalTable { localParams :: [ProcSymType], localSymbols :: (Map String ProcSymbol) }
instance Show LocalTable where
    show (LocalTable params syms) = concat
        [ "params: { "
        , show params
        , "}, locals: {"
        , show $ Map.toList syms
        , "}" ]

-- | Shorthand for the two main types of symbol tables.
type AliasTable = Map String (SourcePos, AliasType)
type ProcTable = Map String (SourcePos, LocalTable)

-- | The root symbol table contains a table of aliases and procedures.
data RootTable = RootTable
    { rootAliases :: AliasTable
    , rootProcs :: ProcTable }

-- | For debugging.
unsafePrintSymbols :: RootTable -> ()
unsafePrintSymbols syms = unsafePerformIO $ do
    pPrint (Map.toList $ rootAliases syms)
    pPrint (Map.toList $ rootProcs syms)

-- | Errors are collected together with the tables in the state monad.
data AliasSymbolState = AliasSymbolState
    { asErrors :: [AnalysisError]
    , asTable  :: AliasTable }

data RootProcSymbolState = RootProcSymbolState
    { rpsErrors :: [AnalysisError]
    , rpsTable  :: ProcTable }

-- | Procedures additionally need to track the stack location being used and the parameters' types.
data ProcSymbolState = ProcSymbolState
    { location :: Int
    , psErrors :: [AnalysisError]
    , psTable  :: Map String ProcSymbol
    , psParams :: [ProcSymType] }

instance EitherState AliasTable AliasSymbolState where
    stateResult state
        | length (asErrors state) > 0 = Left (asErrors state)
        | otherwise = Right (asTable state)

instance EitherState ProcTable RootProcSymbolState where
    stateResult state
        | length (rpsErrors state) > 0 = Left (rpsErrors state)
        | otherwise = Right (rpsTable state)

getAllSymbols :: Program -> Either [AnalysisError] RootTable
getAllSymbols (Program _ arrays procs) = do
    let aliasResult = runEitherState (mapM_ symbolsArray arrays) (AliasSymbolState [] Map.empty)
    (aliases, errs) <- case aliasResult of
        Left aliasErrs -> Right (Map.empty, aliasErrs)
        Right values   -> Right (values, [])
    
    let symbols = RootTable aliases Map.empty
    procs' <- runEitherState (mapM_ (symbolsProc symbols) procs) (RootProcSymbolState errs Map.empty)
    
    let symbols' = RootTable aliases procs'
    return symbols'

getAliasedType :: RootTable -> LocatedTypeName -> Either [AnalysisError] (SourcePos, Type)
getAliasedType _ (LocatedTypeName pos (PrimitiveTypeName RawBoolType)) = Right (pos, TBool)
getAliasedType _ (LocatedTypeName pos (PrimitiveTypeName RawIntType)) = Right (pos, TInt)
getAliasedType (RootTable aliases _) (LocatedTypeName pos (AliasTypeName name)) =
    case Map.lookup (fromIdent name) aliases of
        Just (_, ty) -> Right $ (pos, liftAlias ty)
        Nothing      -> liftSingleErr $ fromSourcePos pos $ "unrecognised type alias `" ++ (fromIdent name) ++ "`"

getPrimitiveType :: LocatedTypeName -> Either [AnalysisError] Type
getPrimitiveType (LocatedTypeName _ (PrimitiveTypeName RawBoolType)) = Right TBool
getPrimitiveType (LocatedTypeName _ (PrimitiveTypeName RawIntType)) = Right TInt
getPrimitiveType (LocatedTypeName pos (AliasTypeName name)) =
    liftSingleErr $ fromSourcePos pos $ "expecting primitive type, found `" ++ (fromIdent name) ++ "`"

symbolsArray :: ArrayType -> State AliasSymbolState ()
symbolsArray (ArrayType _ size ty (Ident pos name)) = do
    current <- get

    case checkExisting current of
        Left errs -> do
            let errs' = (asErrors current) ++ errs
            put (current { asErrors = errs' })
        Right val -> do
            let table = asTable current
            put (current { asTable = Map.insert name val table })
    
    where
        checkExisting current = case Map.lookup name (asTable current) of
            Just (otherPos, _) ->
                Left $ [ fromSourcePosRaw pos $ "redeclaration of type alias `" ++ name ++ "`"
                       , fromSourcePosNote otherPos $ "first declared here:" ]
            Nothing -> do
                ty' <- getPrimitiveType ty
                Right $ (pos, AliasArray size ty')

-- TODO: Records

symbolsProc :: RootTable -> Procedure -> State RootProcSymbolState ()
symbolsProc symbols (Procedure _ (ProcHeader (Ident pos name) params) decls _) = do
    current <- get
    let parent = rpsTable current
    let prevErrs = rpsErrors current

    let initial = ProcSymbolState 0 [] Map.empty []
    let (_, state) = runState (mapM (symbolsParam symbols) params) initial
    let (_, state') = runState (mapM (symbolsDecl symbols) decls) state
    let errs = psErrors state'
    let params = psParams state'
    let table = psTable state'

    -- Check if there is another procedure with this name
    case Map.lookup name parent of
        Just (otherPos, _) -> do
            let newErrs =
                    [ fromSourcePosRaw pos $ "redeclaration of procedure `" ++ name ++ "`"
                    , fromSourcePosNote otherPos $ "first declared here:" ]
            put (current { rpsErrors = prevErrs ++ newErrs ++ errs })

        Nothing -> put (current
            { rpsTable = Map.insert name (pos, LocalTable params table) parent
            , rpsErrors = prevErrs ++ errs })

procCheckExisting :: RootTable -> LocatedTypeName -> Ident -> ProcSymbolState -> Either [AnalysisError] Type
procCheckExisting symbols ty (Ident pos name) current = case Map.lookup name (psTable current) of
    Just other -> let otherPos = symPos other in
        Left $ [ fromSourcePosRaw pos $ "redeclaration of local variable `" ++ name ++ "`"
               , fromSourcePosNote otherPos $ "first declared here:" ]
    Nothing -> do
        (_, ty') <- getAliasedType symbols ty
        Right ty'

symbolsParam :: RootTable -> Parameter -> State ProcSymbolState ()
symbolsParam symbols param = do
    current <- get
    case procCheckExisting symbols ty (Ident pos name) current of
        Left errs -> do
            let errs' = (psErrors current) ++ errs
            put (current { psErrors = errs' })

        Right ty' -> do
            let loc = location current
            let table = psTable current
            let params = psParams current
            let ty'' = cons ty'
            put (current
                { psTable = Map.insert name (ProcSymbol ty'' loc pos) table
                , psParams = params ++ [ty'']
                , location = loc + 1 })

    where
        (cons, ty, pos, name) = case param of
            TypeParam ty (Ident pos name) -> (RefSymbol, ty, pos, name)
            ValParam ty  (Ident pos name) -> (ValSymbol, ty, pos, name)

symbolsDecl :: RootTable -> VarDecl -> State ProcSymbolState ()
symbolsDecl symbols (VarDecl ty idents) = do
    current <- get
    let flattened = zip (cycle [ty]) idents
    let (_, final) = runState (mapM (uncurry symbolsDeclSingle) flattened) current
    put final

    where
        symbolsDeclSingle ty ident@(Ident pos name) = do
            current <- get
            case procCheckExisting symbols ty ident current of
                Left errs -> do
                    let errs' = (psErrors current) ++ errs
                    put (current { psErrors = errs' })

                Right ty -> do
                    let loc = location current
                    let table = psTable current
                    put (current
                        { psTable = Map.insert name (ProcSymbol (ValSymbol ty) loc pos) table
                        , location = loc + 1 })
