module Common where

import Control.Monad.State
import Data.Either (lefts, rights)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

-- | Concatenates a list of pairs of lists.
concatPair ::[([a], [b])] -> ([a], [b])
concatPair = foldr (\(nextA, nextB) (accA, accB) -> (nextA <> accA, nextB <> accB)) ([], [])

-- | The different data types.
data Type = TBool | TString | TInt | TArray Integer Type | TRecord (Map String Type)
    deriving Eq

instance Show Type where
    show TBool   = "boolean"
    show TString = "string"
    show TInt    = "integer"
    show (TArray size ty) = show ty <> "[" <> show size <> "]"
    show (TRecord ty) = show ty

-- | Specific types for aliases allow us to check more correctness without as much
--   clumsy conversion.
data AliasType = AliasArray Integer Type | AliasRecord (Map String Type)
    deriving Eq

instance Show AliasType where
    show (AliasArray size ty) = show ty <> "[" <> show size <>"]" 
    show (AliasRecord record) = concat
        [ "record {"
        , Map.foldr (\next acc -> acc <> ", " <> show next) "" record
        , "}" ]

-- | Converts an AliasType into a general Type.
liftAlias :: AliasType -> Type
liftAlias (AliasArray size ty) = TArray size ty
liftAlias (AliasRecord ty) = TRecord ty

-- | Represents an error during static analysis. Fields are: line, col, message
data AnalysisError = AnalysisError Int Int String | AnalysisNote Int Int String
    deriving Show

-- | Creates an AnalysisError from a given SourcePos (provided by Parsec),
--   and wraps it in an Either.
errorPos :: SourcePos -> String -> Either AnalysisError a
errorPos pos err = Left $ AnalysisError (sourceLine pos)  (sourceColumn pos) err

-- | Creates an AnalysisError from a given SourcePos with a note giving more detail
--   at another SourcePos.
errorWithNote :: SourcePos -> String -> SourcePos -> String -> [AnalysisError]
errorWithNote errPos err notePos note =
    [ AnalysisError (sourceLine errPos)  (sourceColumn errPos)  err
    , AnalysisNote  (sourceLine notePos) (sourceColumn notePos) note ]

-- | Map the error part of the Either.
mapErr :: (a -> c) -> Either a b -> Either c b
mapErr f (Left err)  = Left (f err)
mapErr _ (Right val) = Right val

-- | Lift a single error into a singleton list.
liftOne :: Either a b -> Either [a] b
liftOne = mapErr pure

-- | If Just x is given produces a list from x, otherwise returns an empty list.
ifJust :: Maybe a -> (a -> [b]) -> [b]
ifJust = flip concatMap

concatEither :: [Either [a] [b]] -> Either [a] [b]
concatEither list
        | null lefts' = Right rights'
        | otherwise  = Left lefts'
    where
        lefts'  = concat $ lefts list
        rights' = concat $ rights list

-- | Wrap the notion of a state that also keeps track of errors, with helper functions.
type EitherState s v = State ([AnalysisError], s) v

-- | Add the list of errors to the current EitherState.
addErrors :: [AnalysisError] -> EitherState s ()
addErrors errs = do
    (prevErrs, state) <- get
    put (prevErrs <> errs, state)

-- | Add the list of errors to the current EitherState, or if there are no errors perform
--   an action.
addErrorsOr :: Either [AnalysisError] a -> (a -> EitherState s ()) -> EitherState s ()
addErrorsOr (Left errs) _ = addErrors errs
addErrorsOr (Right val) f = f val

-- | `get` for EitherState.
getEither :: EitherState s s
getEither = gets snd

-- | `put` for EitherState.
putEither :: s -> EitherState s ()
putEither state = do
    (errs, _) <- get
    put (errs, state)

-- | `execState` for EitherState.
execEither :: EitherState s a -> s -> ([AnalysisError], s)
execEither state initial = execState state ([], initial)

runEither :: EitherState s a -> s -> Either [AnalysisError] (a, s)
runEither state initial
    | null errs = Right (val, final)
    | otherwise = Left errs
    where
        (val, (errs, final)) = runState state ([], initial)
