{-# LANGUAGE OverloadedStrings #-}

module SymTable where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

import Common
import RooAst

-- | 

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
data ProcSymbol = ProcSymbol
    { symType :: ProcSymType
    , symLocation :: StackSlot
    , symPos :: SourcePos
    , symName :: Text }
    deriving Eq

rawSymType :: ProcSymbol -> Type
rawSymType = procSymType . symType

instance Show ProcSymbol where
    show sym = concat
        [ show $ symType sym
        , " "
        , T.unpack $ symName sym
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
    { localParams :: [ProcSymbol]
    , localSymbols :: Map Text ProcSymbol }

instance Show LocalTable where
    show (LocalTable params syms) = concat
        [ "params: { "
        , show params
        , "}, locals: {"
        , show $ Map.toList syms
        , "}" ]

-- | Shorthand for the two main types of symbol tables.
type AliasTable = Map Text (SourcePos, AliasType)
type ProcTable = Map Text (SourcePos, LocalTable)

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

lookupProc :: RootTable -> Text -> Maybe (SourcePos, LocalTable)
lookupProc symbols procName = Map.lookup procName (rootProcs symbols)

-- | Procedures additionally need to track the stack location being used and the parameters' types.
data ProcSymbolState = ProcSymbolState
    { location :: Int
    , psTable  :: Map Text ProcSymbol
    , psParams :: [ProcSymbol] }

-- | Analyse a program, and return a symbol table (collecting errors as we go).
getAllSymbols :: Program -> ([AnalysisError], RootTable)
getAllSymbols (Program records arrays procs) = do
    let (errs, records') = execEither (mapM_ symbolsRecord records) Map.empty
    let (errs', aliases) = execEither (mapM_ symbolsArray arrays) records'
    let symbols = RootTable aliases Map.empty

    let (errs'', procs') = execEither (mapM_ (symbolsProc symbols) procs) Map.empty    
    let symbols = RootTable aliases procs'
    
    (errs <> errs' <> errs'', symbols)

-- | Looks up a possibly-aliased type and ensures it is well-formed.
getAliasedType :: RootTable -> LocatedTypeName -> Either [AnalysisError] (SourcePos, Type)
getAliasedType _ (LocatedTypeName pos (PrimitiveTypeName RawBoolType)) = Right (pos, TBool)
getAliasedType _ (LocatedTypeName pos (PrimitiveTypeName RawIntType)) = Right (pos, TInt)
getAliasedType (RootTable aliases _) (LocatedTypeName pos (AliasTypeName (Ident _ name))) =
    case Map.lookup name aliases of
        Just (_, ty) -> Right (pos, liftAlias ty)
        Nothing      -> liftOne $ errorPos pos $
            "unrecognised type alias `" <> name <> "`"

-- | Ensures the given type is primitive.
getPrimitiveType :: LocatedTypeName -> Either [AnalysisError] Type
getPrimitiveType (LocatedTypeName _ (PrimitiveTypeName RawBoolType)) = Right TBool
getPrimitiveType (LocatedTypeName _ (PrimitiveTypeName RawIntType)) = Right TInt
getPrimitiveType (LocatedTypeName pos (AliasTypeName (Ident _ name))) =
    liftOne $ errorPos pos $
        "expecting primitive type, found `" <> name <> "`"

-- | Analyse a single array type declaration and extract any symbols.
symbolsArray :: ArrayType -> EitherState AliasTable ()
symbolsArray (ArrayType size ty (Ident pos name)) = do
    table <- getEither

    addErrorsOr (checkExisting table) (\val -> putEither $ Map.insert name val table)
    
    where
        checkExisting table = case Map.lookup name table of
            Just (otherPos, _) -> Left $
                errorWithNote pos      ("redeclaration of type alias `" <> name <> "`")
                              otherPos "first declared here:"
            Nothing -> do
                ty' <- getPrimitiveType ty
                Right (pos, AliasArray size ty')

-----------------------------------
-- Records
-----------------------------------
-- Analyse a single record type and extract any symbols.
-- WIthin a given record, field names must be distinct 
symbolsRecord :: Record -> EitherState AliasTable ()
symbolsRecord (Record fieldDecls (Ident pos name)) = do 
    table <- getEither  

    -- | Check that the field names are unique and  
    -- | TODO: fix this section 
    let (errs, fieldDecls') = execEither (mapM_ checkFieldDecl fieldDecls) Map.empty 
    addErrors errs 

    case Map.lookup name table of
        Just (otherPos, _) -> addErrors $
                errorWithNote pos      ("redeclaration of type alias `" <> name <> "`")
                              otherPos "first declared here:"
        Nothing -> putEither $ Map.insert name (pos, AliasRecord fieldDecls') table

-- | Check field declaration is correct, update our fieldDecls table and return any errors 
checkFieldDecl :: FieldDecl -> EitherState (Map Text (SourcePos, Type)) ()
checkFieldDecl (FieldDecl ty (Ident pos name)) = do 
    fieldDecls <- getEither
    addErrorsOr (checkExisting fieldDecls) (\val -> putEither $ Map.insert name val fieldDecls)

    where 
        checkExisting fieldDecls = case Map.lookup name fieldDecls of 
            Just (otherPos, _) -> Left $
                errorWithNote pos      ("redeclaration of field name `" <> name <> "`")
                              otherPos "first declared here:"
            Nothing -> Right (pos, liftPrimitive ty)

-----------------------------------
-- Procedures
-----------------------------------
-- | Analyse a single procedure declaration and extract any symbols.
symbolsProc :: RootTable -> Procedure -> EitherState ProcTable ()
symbolsProc symbols (Procedure _ (ProcHeader (Ident pos name) params) decls _) = do
    table <- getEither

    let initial = ProcSymbolState 0 Map.empty []
    let (errs, procSymbols) = execEither (mapM_ (symbolsParam symbols) params) initial
    addErrors errs

    let (errs, procSymbols') = execEither (mapM_ (symbolsDecl symbols) decls) procSymbols
    addErrors errs

    let params = psParams procSymbols'
    let procs = psTable procSymbols'

    -- Check if there is another procedure with this name
    case Map.lookup name table of
        Just (otherPos, _) -> addErrors $
                errorWithNote pos      ("redeclaration of procedure `" <> name <> "`")
                              otherPos  "first declared here:"
        _ -> putEither $ Map.insert name (pos, LocalTable params procs) table

-- | Check whether there is an existing type with this name. If not, returns the checked type.
procCheckExisting :: RootTable -> LocatedTypeName -> Ident -> ProcSymbolState
    -> Either [AnalysisError] Type
procCheckExisting symbols ty (Ident pos name) current
    = case Map.lookup name (psTable current) of
        Just other -> let otherPos = symPos other in Left $
                errorWithNote pos      ("redeclaration of local variable `" <> name <> "`")
                              otherPos "first declared here:"
        Nothing -> do
            (_, ty') <- getAliasedType symbols ty
            Right ty'

-- | Analyse a single formal parameter declaration and extract any symbols.
symbolsParam :: RootTable -> Parameter -> EitherState ProcSymbolState ()
symbolsParam symbols param = do
    procSymbols <- getEither
    
    let result = procCheckExisting symbols ty (Ident pos name) procSymbols

    addErrorsOr result $ \ty -> do
        let loc    = location procSymbols
        let table  = psTable procSymbols
        let params = psParams procSymbols
        let ty'    = cons ty
        let sym = ProcSymbol ty' (StackSlot loc) pos name

        putEither (procSymbols
            { psTable = Map.insert name sym table
            , psParams = params <> [sym]
            , location = loc + 1 })
    where
        (cons, ty, pos, name) = case param of
            TypeParam ty (Ident pos name) -> (RefSymbol, ty, pos, name)
            ValParam ty  (Ident pos name) -> (ValSymbol, ty, pos, name)
            -- TODO: array/record types can't be value params -> should give error

-- | Analyse a single local variable declaration and extract any symbols.
symbolsDecl :: RootTable -> VarDecl -> EitherState ProcSymbolState ()
symbolsDecl symbols (VarDecl ty idents) = do
    procSymbols <- getEither
    
    let flattened = zip (repeat ty) idents
    let (errs, procSymbols') = execEither (mapM_ symbolsDeclSingle flattened) procSymbols

    addErrors errs
    putEither procSymbols'

    where
        symbolsDeclSingle (ty, ident@(Ident pos name)) = do
            procSymbols <- getEither
            let result = procCheckExisting symbols ty ident procSymbols
            
            addErrorsOr result $ \ty -> do
                let loc = location procSymbols
                let table = psTable procSymbols
                putEither (procSymbols
                    { psTable = Map.insert name (ProcSymbol (ValSymbol ty) (StackSlot loc) pos name) table
                    , location = loc + sizeof ty })
