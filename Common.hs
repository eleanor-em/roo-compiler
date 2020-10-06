module Common where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Control.Monad.State

-- | 
concatPair ::[([a], [b])] -> ([a], [b])
concatPair = foldr (\(nextA, nextB) (accA, accB) -> (nextA <> accA, nextB <> accB)) ([], [])

-- | The different data types.
data Type = TBool | TString | TInt | TArray Integer Type | TRecord RecordType
    deriving Eq

instance Show Type where
    show TBool   = "boolean"
    show TString = "string"
    show TInt    = "integer"
    show (TArray size ty) = show ty <> "[" <> show size <> "]"
    show (TRecord ty) = show ty

-- | Specific types for aliases allow us to check more correctness without as much
--   clumsy conversion.
data AliasType = AliasArray Integer Type | AliasRecord RecordType
    deriving (Show, Eq)

liftAlias :: AliasType -> Type
liftAlias (AliasArray size ty) = TArray size ty
liftAlias (AliasRecord ty) = TRecord ty

newtype RecordType = RecordType (Map String Type)
    deriving Eq

instance Show RecordType where
    show (RecordType record) = concat
        [ "record {"
        , Map.foldr (\next acc -> acc <> ", " <> show next) "" record
        , "}" ]

-- | Represents an error during static analysis. Fields are: line, col, message
data AnalysisError = AnalysisError Int Int String | AnalysisNote Int Int String
    deriving Show

-- | Creates an AnalysisError from a given SourcePos (provided by Parsec).
fromSourcePos :: SourcePos -> String -> Either AnalysisError a
fromSourcePos pos err = Left $ fromSourcePosRaw pos err

fromSourcePosRaw :: SourcePos -> String -> AnalysisError
fromSourcePosRaw pos = AnalysisError (sourceLine pos) (sourceColumn pos)

fromSourcePosNote :: SourcePos -> String -> AnalysisError
fromSourcePosNote pos = AnalysisNote (sourceLine pos) (sourceColumn pos)


-- | Combine a list of Eithers into an Either of lists, combining the values using the given rule.
combineErrorsWith :: (b -> b -> b) -> b -> [Either [a] b] -> Either [a] b
combineErrorsWith fold initial list = foldr combine (Right initial) list
    where
        combine (Right val) (Right existing) = Right $ fold val existing
        combine (Left errs) (Right _) = Left errs
        combine (Left errs) (Left list) = Left $ errs <> list
        combine (Right _)   (Left list) = Left list


-- | Map the error part of the Either. Works like a left-map.
mapErr :: (a -> c) -> Either a b -> Either c b
mapErr f (Left err) = Left (f err)
mapErr _ (Right val) = Right val

liftOne :: Either a b -> Either [a] b
liftOne = mapErr pure

liftSingleVal :: Either a b -> Either a [b]
liftSingleVal = fmap pure

liftSingleBoth :: Either a b -> Either [a] [b]
liftSingleBoth (Left err) = Left [err]
liftSingleBoth (Right x) = Right [x]

-- | If Just x is given produces a list from x, otherwise returns an empty list.
ifJust :: Maybe a -> (a -> [b]) -> [b]
ifJust = flip concatMap

type EitherState s v = State ([AnalysisError], s) v

addErrors :: [AnalysisError] -> EitherState s ()
addErrors errs = do
    (prevErrs, state) <- get
    put (prevErrs <> errs, state)

addErrorsOr :: Either [AnalysisError] a -> (a -> EitherState s ()) -> EitherState s ()
addErrorsOr (Left errs) _ = addErrors errs
addErrorsOr (Right val) f = f val

getEither :: EitherState s s
getEither = do
    (_, current) <- get
    return current

putEither :: s -> EitherState s ()
putEither state = do
    (errs, _) <- get
    put (errs, state)

runEitherState :: EitherState s () -> s -> ([AnalysisError], s)
runEitherState state initial = execState state ([], initial)
