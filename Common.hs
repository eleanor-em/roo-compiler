module Common where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec (SourcePos, sourceLine, sourceColumn)
import Control.Monad.State

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

find :: Eq a => a -> [a] -> Maybe a
find _ [] = Nothing
find needle (h:haystack)
    | needle == h = Just needle
    | otherwise   = find needle haystack

-- | The different data types.
data Type = TBool | TString | TInt | TArray Integer Type | TRecord RecordType
    deriving Eq

instance Show Type where
    show TBool   = "boolean"
    show TString = "string"
    show TInt    = "integer"
    show (TArray size ty) = show ty ++ "[" ++ show size ++ "]"
    show (TRecord ty) = show ty

-- | Specific types for aliases allow us to check more correctness without as much
--   clumsy conversion.
data AliasType = AliasArray Integer Type | AliasRecord RecordType
    deriving (Show, Eq)

liftAlias :: AliasType -> Type
liftAlias (AliasArray size ty) = TArray size ty
liftAlias (AliasRecord ty) = TRecord ty

data RecordType = RecordType (Map String Type)
    deriving Eq

instance Show RecordType where
    show (RecordType record) = concat
        [ "record {"
        , Map.foldr (\next acc -> acc ++ ", " ++ show next) "" record
        , "}" ]

-- | Represents an error during static analysis. Fields are: line, col, message
data AnalysisError = AnalysisError Int Int String | AnalysisNote Int Int String
    deriving Show

-- | Creates an AnalysisError from a given SourcePos (provided by Parsec).
fromSourcePos :: SourcePos -> String -> Either AnalysisError a
fromSourcePos pos err = Left $ fromSourcePosRaw pos err

fromSourcePosRaw :: SourcePos -> String -> AnalysisError
fromSourcePosRaw pos err = AnalysisError (sourceLine pos) (sourceColumn pos) err

fromSourcePosNote :: SourcePos -> String -> AnalysisError
fromSourcePosNote pos err = AnalysisNote (sourceLine pos) (sourceColumn pos) err


-- | Combine a list of Eithers into an Either of lists, combining the values using the given rule.
combineErrorsWith :: (b -> b -> b) -> b -> [(Either [a] b)] -> Either [a] b
combineErrorsWith fold initial list = foldr combine (Right initial) list
    where
        combine (Right val) (Right existing) = Right $ fold val existing
        combine (Left errs) (Right _) = Left errs
        combine (Left errs) (Left list) = Left $ errs ++ list
        combine (Right _)   (Left list) = Left list


-- | Map the error part of the Either. Works like a left-map.
mapErr :: (a -> c) -> Either a b -> Either c b
mapErr f (Left err) = Left (f err)
mapErr _ (Right val) = Right val

liftSingleErr :: Either a b -> Either [a] b
liftSingleErr = mapErr pure

liftSingleVal :: Either a b -> Either a [b]
liftSingleVal = fmap pure

liftSingleBoth :: Either a b -> Either [a] [b]
liftSingleBoth (Left err) = Left [err]
liftSingleBoth (Right x) = Right [x]

-- | Because our states contain errors, we define a helper class to extract the errors
--   if they exist. If not, there is some defined value i.e. the produced table.
class EitherState v a where
    stateResult :: a -> Either [AnalysisError] v

-- | Analogous to runState, except it produces an Either representing errors or the final value.
runEitherState :: EitherState v s => State s a -> s -> Either [AnalysisError] v
runEitherState state initial = do
    let (_, val) = runState state initial
    stateResult val
