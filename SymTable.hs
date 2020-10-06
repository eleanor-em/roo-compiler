module SymTable where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

import Common
import RooAst

-- | A procedure symbol can be either a value or a reference.
data ProcSymType = ValSymbol Type | RefSymbol Type
    deriving Eq

procSymType :: ProcSymType -> Type
procSymType (ValSymbol ty) = ty
procSymType (RefSymbol ty) = ty

instance Show ProcSymType where
    show (ValSymbol ty) = show ty <> " val"
    show (RefSymbol ty) = show ty <> " ref"

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
data LocalTable = LocalTable
    { localParams :: [ProcSymType]
    , localSymbols :: (Map String ProcSymbol) }

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

instance Show RootTable where
    show syms = concat
        [ "["
        , show $ Map.toList $ rootAliases syms
        , ", "
        , show $ Map.toList $ rootProcs syms
        , "]" ]

-- | Procedures additionally need to track the stack location being used and the parameters' types.
data ProcSymbolState = ProcSymbolState
    { location :: Int
    , psTable  :: Map String ProcSymbol
    , psParams :: [ProcSymType] }

getAllSymbols :: Program -> ([AnalysisError], RootTable)
getAllSymbols (Program _ arrays procs) = do
    let (errs, arrays') = runEitherState (mapM_ symbolsArray arrays) Map.empty
    let symbols = RootTable arrays' Map.empty

    let (errs', procs') = runEitherState (mapM_ (symbolsProc symbols) procs) Map.empty    
    let symbols = RootTable arrays' procs'
    
    (errs <> errs', symbols)

getAliasedType :: RootTable -> LocatedTypeName -> Either [AnalysisError] (SourcePos, Type)
getAliasedType _ (LocatedTypeName pos (PrimitiveTypeName RawBoolType)) = Right (pos, TBool)
getAliasedType _ (LocatedTypeName pos (PrimitiveTypeName RawIntType)) = Right (pos, TInt)
getAliasedType (RootTable aliases _) (LocatedTypeName pos (AliasTypeName (Ident _ name))) =
    case Map.lookup name aliases of
        Just (_, ty) -> Right $ (pos, liftAlias ty)
        Nothing      -> liftOne $ fromSourcePos pos $
            "unrecognised type alias `" <> name <> "`"

getPrimitiveType :: LocatedTypeName -> Either [AnalysisError] Type
getPrimitiveType (LocatedTypeName _ (PrimitiveTypeName RawBoolType)) = Right TBool
getPrimitiveType (LocatedTypeName _ (PrimitiveTypeName RawIntType)) = Right TInt
getPrimitiveType (LocatedTypeName pos (AliasTypeName (Ident _ name))) =
    liftOne $ fromSourcePos pos $
        "expecting primitive type, found `" <> name <> "`"

symbolsArray :: ArrayType -> EitherState AliasTable ()
symbolsArray (ArrayType _ size ty (Ident pos name)) = do
    table <- getEither

    addErrorsOr (checkExisting table) (\val -> putEither $ Map.insert name val table)
    
    where
        checkExisting table = case Map.lookup name table of
            Just (otherPos, _) ->
                Left $ [ fromSourcePosRaw pos $ "redeclaration of type alias `" <> name <> "`"
                       , fromSourcePosNote otherPos $ "first declared here:" ]
            Nothing -> do
                ty' <- getPrimitiveType ty
                Right $ (pos, AliasArray size ty')

-- TODO: Records

symbolsProc :: RootTable -> Procedure -> EitherState ProcTable ()
symbolsProc symbols (Procedure _ (ProcHeader (Ident pos name) params) decls _) = do
    table <- getEither

    let (errs, procSymbols) = runEitherState (mapM_ (symbolsParam symbols) params) (ProcSymbolState 0 Map.empty [])
    addErrors errs

    let (errs, procSymbols') = runEitherState (mapM_ (symbolsDecl symbols) decls) procSymbols
    addErrors errs

    let params = psParams procSymbols'
    let procs = psTable procSymbols'

    -- Check if there is another procedure with this name
    addErrors $ ifJust (Map.lookup name table) $ \(otherPos, _) ->
             [ fromSourcePosRaw pos $ "redeclaration of procedure `" <> name <> "`"
             , fromSourcePosNote otherPos "first declared here:" ]
    addErrors errs
    
    putEither $ Map.insert name (pos, LocalTable params procs) table

procCheckExisting :: RootTable -> LocatedTypeName -> Ident -> ProcSymbolState -> Either [AnalysisError] Type
procCheckExisting symbols ty (Ident pos name) current
    = case Map.lookup name (psTable current) of
        Just other -> let otherPos = symPos other in
            Left $ [ fromSourcePosRaw pos $ "redeclaration of local variable `" <> name <> "`"
                    , fromSourcePosNote otherPos $ "first declared here:" ]
        Nothing -> do
            (_, ty') <- getAliasedType symbols ty
            Right ty'

symbolsParam :: RootTable -> Parameter -> EitherState ProcSymbolState ()
symbolsParam symbols param = do
    procSymbols <- getEither
    
    let result = procCheckExisting symbols ty (Ident pos name) procSymbols

    addErrorsOr result $ \ty -> do
        let loc = location procSymbols
        let table = psTable procSymbols
        let params = psParams procSymbols
        let ty' = cons ty
        putEither (procSymbols
            { psTable = Map.insert name (ProcSymbol ty' loc pos) table
            , psParams = params <> [ty']
            , location = loc + 1 })
    where
        (cons, ty, pos, name) = case param of
            TypeParam ty (Ident pos name) -> (RefSymbol, ty, pos, name)
            ValParam ty  (Ident pos name) -> (ValSymbol, ty, pos, name)

symbolsDecl :: RootTable -> VarDecl -> EitherState ProcSymbolState ()
symbolsDecl symbols (VarDecl ty idents) = do
    procSymbols <- getEither
    
    let flattened = zip (repeat ty) idents
    let (errs, procSymbols') = runEitherState (mapM_ (uncurry symbolsDeclSingle) flattened) procSymbols

    addErrors errs
    putEither procSymbols'

    where
        symbolsDeclSingle ty ident@(Ident pos name) = do
            procSymbols <- getEither
            let result = procCheckExisting symbols ty ident procSymbols
            addErrorsOr result $ \ty -> do
                let loc = location procSymbols
                let table = psTable procSymbols
                putEither (procSymbols
                    { psTable = Map.insert name (ProcSymbol (ValSymbol ty) loc pos) table
                    , location = loc + 1 })
