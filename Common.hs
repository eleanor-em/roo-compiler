{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Common where

import Control.Monad.State
import qualified Data.Bifunctor as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (lefts, rights)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

import RooAst

newtype Register = Register Int
    deriving Eq

instance Show Register where
    show (Register r) = "r" <> show r

newtype StackSlot = StackSlot Int
    deriving (Eq, Num)

stackSlotToInt :: StackSlot -> Int
stackSlotToInt (StackSlot x) = x

instance Show StackSlot where
    show (StackSlot l) = show l

-- | Concatenates a list of pairs of lists.
concatPair ::[([a], [b])] -> ([a], [b])
concatPair = foldr (\(nextA, nextB) (accA, accB) -> (nextA <> accA, nextB <> accB)) ([], [])

tshow :: Show a => a -> Text
tshow = T.pack . show

countWithNoun :: (Show a, Integral a) => a -> Text -> Text
countWithNoun x noun
    | x == 1    = "1 " <> noun
    | otherwise = tshow x <> " " <> noun <> "s"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

leftmap :: B.Bifunctor f => (a -> b) -> f a c -> f b c
leftmap = B.first

data Field = Field
    { fieldPos :: SourcePos
    , fieldOffset :: StackSlot
    , fieldTy :: Type }
    deriving Eq

instance Show Field where
    show = show . fieldTy

-- | The different data types.
data Type = TBool | TString | TInt | TArray Int Type | TRecord (Map Text Field)
    deriving Eq

liftPrimitive :: PrimitiveType -> Type
liftPrimitive RawBoolType = TBool
liftPrimitive RawIntType = TInt

instance Show Type where
    show TBool   = "boolean"
    show TString = "string"
    show TInt    = "integer"
    show (TArray size ty) = show ty <> "[" <> show size <> "]"
    show (TRecord ty) = show ty

sizeof :: Type -> Int
sizeof TBool = 1
sizeof TString = 1
sizeof TInt = 1
sizeof (TArray size ty) = size * sizeof ty
sizeof (TRecord map) = foldr ((+) . sizeof . fieldTy) 0 map

-- | Specific types for aliases allow us to check more correctness without as much
--   clumsy conversion.
data AliasType = AliasArray Int Type | AliasRecord (Map Text Field)
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
data AnalysisError = AnalysisError Int Int Text | AnalysisNote Int Int Text
    deriving Show

-- | Creates an AnalysisError from a given SourcePos (provided by Parsec),
--   and wraps it in an Either.
errorPos :: SourcePos -> Text -> Either AnalysisError a
errorPos pos err = Left $ AnalysisError (sourceLine pos) (sourceColumn pos) err

-- | Creates an AnalysisError from a given SourcePos with a note giving more detail
--   at another SourcePos.
errorWithNote :: SourcePos -> Text -> SourcePos -> Text -> [AnalysisError]
errorWithNote errPos err notePos note =
    [ AnalysisError (sourceLine errPos)  (sourceColumn errPos)  err
    , AnalysisNote  (sourceLine notePos) (sourceColumn notePos) note ]

-- | Lift a single error into a singleton list.
liftOne :: Either a b -> Either [a] b
liftOne = leftmap pure
-- | If Just x is given produces a list from x, otherwise returns an empty list.
ifJust :: Maybe a -> (a -> [b]) -> [b]
ifJust = flip concatMap

-- | concats both left and right, but returns the one that exists 
concatEither :: [Either [a] [b]] -> Either [a] [b]
concatEither list
        | null lefts' = Right rights'
        | otherwise   = Left lefts'
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

includeEither :: Either [AnalysisError] () -> EitherState s ()
includeEither (Left errs) = addErrors errs
includeEither _ = pure ()

-- | `get` for EitherState.
getEither :: EitherState s s
getEither = gets snd

-- | `put` for EitherState.
putEither :: s -> EitherState s ()
putEither state = do
    (errs, _) <- get
    put (errs, state)

(<?>) :: (a -> EitherState s ()) -> Maybe a -> EitherState s ()
f <?> Just x  = f x
_ <?> Nothing = void getEither

-- | `execState` for EitherState. "run the state, return errors and final state"
execEither :: EitherState s a -> s -> ([AnalysisError], s)
execEither state initial = execState state ([], initial)

--  runEither: "run the state, return errors, final state, *and* an extra value"
runEither :: EitherState s a -> s -> Either [AnalysisError] (a, s)
runEither state initial
    | null errs = Right (val, final)
    | otherwise = Left errs
    where
        (val, (errs, final)) = runState state ([], initial)

unwrapOr :: Maybe v -> Either e v -> Either e v
unwrapOr (Just x) _  = Right x
unwrapOr Nothing err = err
