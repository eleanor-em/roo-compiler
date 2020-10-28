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
--   (We could use something like ExceptT, but we only learnt about it later.)
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

-- | `runState` for EitherSTate. "run the state, return errors, final state, *and* an extra value"
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

-- | Concatenates Left and Right lists. If there are any Left values, return Lefts; otherwise,
--   return Rights.
concatEither :: [Either [a] [b]] -> Either [a] [b]
concatEither list
        | null lefts' = Right rights'
        | otherwise   = Left lefts'
    where
        lefts'  = concat $ lefts list
        rights' = concat $ rights list

-----------------------------------
-- Type Declarations 
-----------------------------------

-- | Wrapping all the potential data types under a `Type` 
data Type = TBool
          | TString
          | TInt
          | TVoid
          | TNever
          | TArray Text Int Type
          | TRecord Text (Map Text Field)
          | TFunc [ProcSymType] Type
    deriving (Ord, Eq)

-- | A procedure symbol can be either a value or a reference.
data ProcSymType = ValSymbol Type | RefSymbol Type
    deriving (Ord, Eq)

-- | A Record Field node consists of: 
-- 
--     1. its source position in the source file
--     2. the stack slot that it's stored in
--     3. the type of the field 
data Field = Field
    { fieldPos :: SourcePos
    , fieldOffset :: StackSlot
    , fieldTy :: Type }
    deriving (Ord, Eq)

-- | A Register is a wrapper type to indicate registers in Oz
newtype Register = Register Int
    deriving Eq

-- | A Stackslot is a wrapper type to indicate specific stackslots in oz
newtype StackSlot = StackSlot Int
    deriving (Ord, Eq, Num)

-----------------------------------
-- Type to Text Conversions 
-----------------------------------

-- | Indicating how to `Show` our types when read
instance Show Type where
    show TBool   = "boolean"
    show TString = "string"
    show TInt    = "integer"
    show TVoid   = "void"
    show TNever  = "!"
    show (TArray name size ty) = show name <> " = " <> show ty <> "[" <> show size <> "]"
    show (TRecord name _) = show name <> "{}"
    show (TFunc params ret) = "procedure(" <> concatMap show params <> ") -> " <> show ret

-- | Indicating how to `Show` objects that are pass by ref or local 
instance Show ProcSymType where
    show (ValSymbol ty) = show ty <> " val"
    show (RefSymbol ty) = show ty <> " ref"

-- | Indicating how to `Show` Field objects 
instance Show Field where
    show = show . fieldTy

-- | Indicating how to `Show` Registers 
instance Show Register where
    show (Register r) = "r" <> show r

-- | Indicating how to `Show` Stackslots
instance Show StackSlot where
    show (StackSlot l) = show l

-- | Combining type -> String & String -> Text 
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Visualising `backticks`
backticks :: (Show a) => a -> Text
backticks x = "`" <> tshow x <> "`"

-- | Pluralises a now correctly.
countWithNoun :: (Show a, Integral a) => a -> Text -> Text
countWithNoun x noun
    | x == 1    = "1 " <> noun
    | otherwise = tshow x <> " " <> noun <> "s"

-- | Visualising booleans (the `Show` instance is capitalised which looks wrong)
tshowBool :: Bool -> Text
tshowBool True = "true"
tshowBool False = "false"

-----------------------------------
-- General `Type` Helper Functions 
-----------------------------------
-- | Calculating the size of different types
sizeof :: Type -> Int
sizeof TBool = 1
sizeof TString = 1
sizeof TInt = 1
sizeof (TArray _ size ty) = size * sizeof ty
sizeof (TRecord _ map) = foldr ((+) . sizeof . fieldTy) 0 map
sizeof TVoid = 0
sizeof (TFunc _ _) = 1
sizeof TNever = 0

-- | Unwrapping a StackSlot into an Int 
stackSlotToInt :: StackSlot -> Int
stackSlotToInt (StackSlot x) = x

-- | Lifting a raw primitive into a standard type 
liftPrimitive :: PrimitiveType -> Type
liftPrimitive RawBoolType = TBool
liftPrimitive RawIntType = TInt

-- | Helper function for determining if something is primitive
isPrimitive :: Type -> Bool
isPrimitive TBool = True
isPrimitive TInt = True
isPrimitive _ = False

-- | Helper function for determining if something is a function
isFunction :: Type -> Bool
isFunction (TFunc _ _) = True
isFunction _           = False

-- | Unwrapping a ProcSymType into its underlying type 
procSymType :: ProcSymType -> Type
procSymType (ValSymbol ty) = ty
procSymType (RefSymbol ty) = ty

-----------------------------------
-- General Helper Functions  
-----------------------------------

-- | Unrapping a Maybe object into an Either object 
unwrapOr :: Maybe v -> Either e v -> Either e v
unwrapOr (Just x) _  = Right x
unwrapOr Nothing err = err

-- | Concatenates a list of pairs of lists.
concatPair :: [([a], [b])] -> ([a], [b])
concatPair = foldr combine ([], [])
    where
        combine = \(nextA, nextB) (accA, accB) -> (nextA <> accA, nextB <> accB)

-- | Mapping a function across pairs of obejcts 
mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

-- | Generating a list of pairs, by attaching a number to each object (same as Python)
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- | `uncurry` but for 3-argument functions
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z)= f x y z

-----------------------------------
-- Text processing for prettifying generated Oz code
-----------------------------------

addIndent :: Text -> Text
addIndent "" = ""
addIndent str
    | T.head str == '#' = str
    | otherwise         = "    " <> str

makeComment :: Text -> [Text]
makeComment str = ["# " <> T.replace "\n" "" str]

makeProcLabel :: Text -> Text
makeProcLabel = ("proc_" <>)

makeProcTailLabel :: Text -> Text
makeProcTailLabel name = "proc_" <> name <> "_loaded"

lambdaLabel :: Int -> Text
lambdaLabel i = "__lambda" <> tshow i

vTableLabel :: Text
vTableLabel = "__vtable"
