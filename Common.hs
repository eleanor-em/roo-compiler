{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Common where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (lefts, rights)
import Data.Map.Strict (Map)

import Text.Parsec (SourcePos, sourceLine, sourceColumn)

import RooAst

-----------------------------------
-- EitherState Implementation 
-----------------------------------

-- | A state that also keeps track of errors, with helper functions.
type EitherState s v = State ([AnalysisError], s) v

-- | Represents an error during static analysis. Fields are: line, col, message
data AnalysisError = AnalysisError Int Int Text
                   | AnalysisNote  Int Int Text
                   | AnalysisWarn  Int Int Text
    deriving Show

-- | Add the list of errors to the current EitherState.
addErrors :: [AnalysisError] -> EitherState s ()
addErrors errs = do
    (prevErrs, state) <- get
    put (prevErrs <> errs, state)

-- | Add the list of errors to the current EitherState, or if there are no
--   errors perform an action.
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

-----------------------------------
-- Error Analysis Management 
-----------------------------------

-- | Creates an AnalysisError from a given SourcePos (provided by Parsec),
--   and wraps it in an Either.
errorPos :: SourcePos -> Text -> [AnalysisError]
errorPos pos err = pure $ AnalysisError (sourceLine pos) (sourceColumn pos) err

warnPos :: SourcePos -> Text -> [AnalysisError]
warnPos pos warning = pure $ AnalysisWarn (sourceLine pos) (sourceColumn pos) warning

-- | Creates an AnalysisError from a given SourcePos with a note giving more
--   detail at another SourcePos.
errorWithNote :: SourcePos -> Text -> SourcePos -> Text -> [AnalysisError]
errorWithNote errPos err notePos note =
    [ AnalysisError (sourceLine errPos)  (sourceColumn errPos)  err
    , AnalysisNote  (sourceLine notePos) (sourceColumn notePos) note ]

-- | concats both left and right, but returns the one that exists 
concatEither :: [Either [a] [b]] -> Either [a] [b]
concatEither list
        | null lefts' = Right rights'
        | otherwise   = Left lefts'
    where
        lefts'  = concat $ lefts list
        rights' = concat $ rights list


-----------------------------------
-- General Type Management 
-----------------------------------

-- | The different data types.
data Type = TBool
          | TString
          | TInt
          | TArray Text Int Type
          | TRecord Text (Map Text Field)
          | TFunc [ProcSymType] Type
          | TVoid
          | TNever
    deriving (Ord, Eq)

instance Show Type where
    show TBool   = "boolean"
    show TString = "string"
    show TInt    = "integer"
    show (TArray name size ty) = show name <> " = " <> show ty <> "[" <> show size <> "]"
    show (TRecord name _) = show name <> "{}"
    show TVoid   = "void"
    show TNever  = "!"
    show (TFunc params ret) = "procedure(" <> concatMap show params <> ") -> " <> show ret

sizeof :: Type -> Int
sizeof TBool = 1
sizeof TString = 1
sizeof TInt = 1
sizeof (TArray _ size ty) = size * sizeof ty
sizeof (TRecord _ map) = foldr ((+) . sizeof . fieldTy) 0 map
sizeof TVoid = 0
sizeof (TFunc _ _) = 1
sizeof TNever = 0

tshow :: Show a => a -> Text
tshow = T.pack . show

-----------------------------------
-- Record Field Type Management 
-----------------------------------

data Field = Field
    { fieldPos :: SourcePos
    , fieldOffset :: StackSlot
    , fieldTy :: Type }
    deriving (Ord, Eq)

instance Show Field where
    show = show . fieldTy

-----------------------------------
-- Procedure Sym Type Management 
-----------------------------------

-- | A procedure symbol can be either a value or a reference.
data ProcSymType = ValSymbol Type | RefSymbol Type
    deriving (Ord, Eq)

procSymType :: ProcSymType -> Type
procSymType (ValSymbol ty) = ty
procSymType (RefSymbol ty) = ty

instance Show ProcSymType where
    show (ValSymbol ty) = show ty <> " val"
    show (RefSymbol ty) = show ty <> " ref"

-----------------------------------
-- Wrapper Types 
-----------------------------------

newtype Register = Register Int
    deriving Eq

instance Show Register where
    show (Register r) = "r" <> show r

newtype StackSlot = StackSlot Int
    deriving (Ord, Eq, Num)

instance Show StackSlot where
    show (StackSlot l) = show l

stackSlotToInt :: StackSlot -> Int
stackSlotToInt (StackSlot x) = x

-----------------------------------
-- General Helper Functions  
-----------------------------------

unwrapOr :: Maybe v -> Either e v -> Either e v
unwrapOr (Just x) _  = Right x
unwrapOr Nothing err = err

-- | Concatenates a list of pairs of lists.
concatPair :: [([a], [b])] -> ([a], [b])
concatPair = foldr combine ([], [])
    where
        combine = \(nextA, nextB) (accA, accB) -> (nextA <> accA, nextB <> accB)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

tshowBool :: Bool -> Text
tshowBool True = "true"
tshowBool False = "false"

countWithNoun :: (Show a, Integral a) => a -> Text -> Text
countWithNoun x noun
    | x == 1    = "1 " <> noun
    | otherwise = tshow x <> " " <> noun <> "s"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z)= f x y z

liftPrimitive :: PrimitiveType -> Type
liftPrimitive RawBoolType = TBool
liftPrimitive RawIntType = TInt

isPrimitive :: Type -> Bool
isPrimitive TBool = True
isPrimitive TInt = True
isPrimitive _ = False

isFunction :: Type -> Bool
isFunction (TFunc _ _) = True
isFunction _           = False