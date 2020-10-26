{-# LANGUAGE OverloadedStrings #-}

module SymTable where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

import Common
import RooAst
import Text.Parsec.Pos (initialPos)

-- | Represents a procedure symbol with a type, location on the stack, and source position.
data ProcSymbol = ProcSymbol
    { symType :: ProcSymType
    , symLocation :: StackSlot
    , symPos :: SourcePos
    , symName :: Text }
    deriving Eq

toProcSym :: RootTable -> Parameter -> Either [AnalysisError] ProcSymType
toProcSym table (TypeParam ty _) = RefSymbol . snd <$> lookupType table ty
toProcSym table (ValParam ty _)  = ValSymbol . snd <$> lookupType table ty

rawSymType :: ProcSymbol -> Type
rawSymType = procSymType . symType

isValSymbol :: ProcSymbol -> Bool
isValSymbol (ProcSymbol (ValSymbol _) _ _ _) = True
isValSymbol _ = False

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
    , localSymbols :: Map Text ProcSymbol
    , localRetType :: Type
    , localProcName :: Text }

localStackSize :: LocalTable -> Int
localStackSize (LocalTable _ syms _ _) = foldr (\x acc -> actualSize x + acc) 0 syms
    where
        actualSize (ProcSymbol (ValSymbol ty) _ _ _) = sizeof ty
        actualSize _ = 1

-- | Shorthand for the two main types of symbol tables.
type AliasTable = Map Text (SourcePos, Type)
type ProcTable = Map Text (SourcePos, LocalTable)
type FuncPtrTable = Map Text (Int, Type)
type Vtable = Map Int (SourcePos, LocalTable)

-- | The root symbol table contains a table of aliases and procedures.
data RootTable = RootTable
    { rootAliases :: AliasTable
    , rootProcs :: ProcTable
    , rootFuncPtrs :: FuncPtrTable
    , rootVtable :: Vtable }

instance Semigroup RootTable where
    (RootTable lAliases lProcs lFuncPtrs lVtable) <> (RootTable rAliases rProcs rFuncPtrs rVtable)
        = RootTable (Map.union lAliases rAliases)
                    (Map.union lProcs rProcs)
                    (Map.union lFuncPtrs rFuncPtrs)
                    (Map.union lVtable rVtable)

instance Monoid RootTable where
    mempty = RootTable Map.empty Map.empty Map.empty Map.empty


data RecordSymbolState = RecordSymbolState
    { rsOffset :: Int
    , rsTable :: Map Text Field }

lookupProc :: RootTable -> Text -> Maybe (SourcePos, LocalTable)
lookupProc symbols procName = Map.lookup procName (rootProcs symbols)

data ProcState = ProcState
    { procTable :: ProcTable
    , procFuncPtrs :: FuncPtrTable
    , procVtable :: Vtable }

initialProcState :: ProcState
initialProcState = ProcState Map.empty Map.empty Map.empty

-- | Procedures additionally need to track the stack location being used and the parameters' types.
data ProcSymbolState = ProcSymbolState
    { location :: Int
    , psTable  :: Map Text ProcSymbol
    , psParams :: [ProcSymbol] }

-- | Analyse a program, and return a symbol table (collecting errors as we go).
getAllSymbols :: Program -> RootTable -> ([AnalysisError], RootTable)
getAllSymbols (Program records arrays procs) initial = do
    let (errs, records') = execEither (mapM_ symbolsRecord records) (rootAliases initial)
    let symbols = initial <> RootTable records' Map.empty Map.empty Map.empty

    let (errs', aliases) = execEither (mapM_ (symbolsArray symbols) arrays) records'
    let symbols = initial <> RootTable aliases Map.empty Map.empty Map.empty

    let (errs'', ProcState procs' funcPtrs vtable) = execEither (mapM_ (symbolsProc symbols) procs)
                                                                initialProcState

    let symbols = initial <>RootTable aliases procs' funcPtrs vtable
    
    (errs <> errs' <> errs'', symbols)

-- | Looks up a possibly-aliased type and ensures it is well-formed.
lookupType :: RootTable -> LocatedTypeName -> Either [AnalysisError] (SourcePos, Type)
lookupType _ (LocatedTypeName pos (PrimitiveTypeName RawBoolType)) = Right (pos, TBool)
lookupType _ (LocatedTypeName pos (PrimitiveTypeName RawIntType))  = Right (pos, TInt)
lookupType _ (LocatedTypeName pos VoidTypeName) = Right (pos, TVoid)

lookupType (RootTable aliases _ _ _) (LocatedTypeName pos (AliasTypeName (Ident _ name))) =
    case Map.lookup name aliases of
        Just (_, ty) -> Right (pos, ty)
        Nothing      -> Left  $ errorPos pos $
            "unrecognised type alias `" <> name <> "`"

lookupType table (LocatedTypeName pos (FunctionTypeName params retType)) = do
    paramTys <- mapM (toProcSym table) params
    let retType' = case retType of
            PrimitiveTypeName ty -> liftPrimitive ty
            _                    -> TVoid
    return (pos, TFunc paramTys retType')

-- | Analyse a single array type declaration and extract any symbols.
symbolsArray :: RootTable -> ArrayType -> EitherState AliasTable ()
symbolsArray rootSyms (ArrayType size ty (Ident pos name)) = do
    table <- getEither

    addErrorsOr (checkExisting table) (\val -> putEither $ Map.insert name val table)
    
    where
        checkExisting table = case Map.lookup name table of
            Just (otherPos, _) -> Left $
                errorWithNote pos      ("redeclaration of type alias `" <> name <> "`")
                              otherPos "first declared here:"
            Nothing -> do
                (_, ty') <- lookupType rootSyms ty
                Right (pos, TArray name size ty')

-----------------------------------
-- Records
-----------------------------------
-- Analyse a single record type and extract any symbols.
-- WIthin a given record, field names must be distinct 
symbolsRecord :: Record -> EitherState AliasTable ()
symbolsRecord (Record fieldDecls (Ident pos name)) = do 
    table <- getEither  

    let (errs, final) = execEither (mapM_ checkFieldDecl fieldDecls) (RecordSymbolState 0 Map.empty)
    addErrors errs 

    case Map.lookup name table of
        Just (otherPos, _) -> addErrors $
                errorWithNote pos      ("redeclaration of type alias `" <> name <> "`")
                              otherPos "first declared here:"
        Nothing -> putEither $ Map.insert name (pos, TRecord name (rsTable final)) table

-- | Check field declaration is correct, update our fieldDecls table and return any errors 
checkFieldDecl :: FieldDecl -> EitherState RecordSymbolState ()
checkFieldDecl (FieldDecl ty (Ident pos name)) = do 
    current <- getEither
    let offset = rsOffset current
    let table  = rsTable  current
    
    case Map.lookup name table of 
        Just (Field otherPos _ _) -> addErrors $
            errorWithNote pos      ("redeclaration of field name `" <> name <> "`")
                          otherPos "first declared here:"
        Nothing -> putEither $ current
            { rsOffset = offset + sizeof (liftPrimitive ty)
            , rsTable = Map.insert name (Field pos (StackSlot offset) (liftPrimitive ty)) table }

-----------------------------------
-- Procedures
-----------------------------------
-- | Analyse a single procedure declaration and extract any symbols.
symbolsProc :: RootTable -> Procedure -> EitherState ProcState ()
symbolsProc symbols (Procedure _ (ProcHeader (Ident pos name) params) retType decls _) = do
    (ProcState table funcPtrs vtable) <- getEither
    
    let initial = ProcSymbolState 0 Map.empty []
    let (errs, procSymbols) = execEither (mapM_ (symbolsParam symbols) params) initial
    addErrors errs

    let (errs, procSymbols') = execEither (mapM_ (symbolsDecl symbols) decls) procSymbols
    addErrors errs

    let params' = psParams procSymbols'
    let procs'  = psTable procSymbols'

    -- Check if there is another procedure with this name
    case Map.lookup name table of
        Just (otherPos, _) -> addErrors $
                errorWithNote pos      ("redeclaration of procedure `" <> name <> "`")
                              otherPos  "first declared here:"
        _ -> do
            let myProc = (pos, LocalTable params' procs' retType' name)
            let newProcs = Map.insert name myProc table

            let myType = LocatedTypeName (initialPos "") (FunctionTypeName params retType)
            let (newPtrs, newVtable) = case lookupType symbols myType of
                    Left  _       -> (funcPtrs, vtable)
                    Right (_, ty) -> do
                        let index = Map.size funcPtrs
                        let newPtrs = Map.insert name (index, ty) funcPtrs
                        let newVtable = Map.insert index myProc vtable
                        (newPtrs, newVtable)

            putEither (ProcState newProcs newPtrs newVtable)
    where
        retType' = case retType of
            PrimitiveTypeName ty -> liftPrimitive ty
            _                    -> TVoid

-- | Check whether there is an existing type with this name. If not, returns the checked type.
procCheckExisting :: RootTable -> LocatedTypeName -> Ident -> ProcSymbolState
    -> Either [AnalysisError] Type
procCheckExisting symbols ty (Ident pos name) current
    = case Map.lookup name (psTable current) of
        Just other -> let otherPos = symPos other in Left $
                errorWithNote pos      ("redeclaration of local variable `" <> name <> "`")
                              otherPos "first declared here:"
        Nothing -> do
            (_, ty') <- lookupType symbols ty
            Right ty'

typeOrNever :: Either [AnalysisError] Type -> EitherState ProcSymbolState Type
typeOrNever result = case result of
    Left errs -> do
        addErrors errs
        return TNever
    Right ty -> pure ty

-- | Analyse a single formal parameter declaration and extract any symbols.
symbolsParam :: RootTable -> Parameter -> EitherState ProcSymbolState ()
symbolsParam symbols param = do
    procSymbols <- getEither
    
    ty <- typeOrNever $ procCheckExisting symbols ty (Ident pos name) procSymbols

    let loc    = location procSymbols
    let table  = psTable procSymbols
    let params = psParams procSymbols
    let ty'    = cons ty
    let sym = ProcSymbol ty' (StackSlot loc) pos name

    putEither (procSymbols
        { psTable = Map.insert name sym table
        , psParams = params <> [sym]
        , location = loc + 1 }) -- formal params are either primitive or references => size 1
    where
        (cons, ty, pos, name) = case param of
            TypeParam ty (Ident pos name) -> (RefSymbol, ty, pos, name)
            ValParam  ty (Ident pos name) -> (ValSymbol, ty, pos, name)

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
            ty <- typeOrNever $ procCheckExisting symbols ty ident procSymbols

            let loc = location procSymbols
            let table = psTable procSymbols
            let sym = (ProcSymbol (ValSymbol ty) (StackSlot loc) pos name)

            putEither (procSymbols
                { psTable = Map.insert name sym table
                , location = loc + sizeof ty })
