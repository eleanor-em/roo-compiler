{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : RooPreprocessor
Description : Handles some pre-compilation steps  

-}

module RooPreprocessor where

import Text.Regex
import RooAst 
import Common 
import Control.Monad.State 
import qualified Data.Map.Strict as Map
import SymTable (RootTable, localProcName, rootVtable)
import Data.Text (Text)
import Oz 

-----------------------------------
-- Managing @Includes 
-----------------------------------

includeMatcher :: Regex
includeMatcher = mkRegexWithOpts "^@include *\"(.+)\" *$" True True

extractIncludes :: String -> Maybe [String]
extractIncludes = matchRegex includeMatcher

removeIncludes :: String -> String
removeIncludes str = subRegex includeMatcher str ""

-----------------------------------
-- Compiling Lambda Expressions
-----------------------------------

-- | Searches a procedure for any lambda functions, turning them into procedure definitions.
compileLambdas :: Procedure -> State Int [Procedure]
compileLambdas (Procedure _ _ _ _ statements)
    = do
        procs <- mapM compileLambdasInner statements
        return $ concat procs

-- | Search the statement for any lambda functions, and turn them into procedure definitions.
--   The state encodes the current used index.
compileLambdasInner :: Statement -> State Int [Procedure]
compileLambdasInner (SAssign _ rhs) 
    = compileExprLambdas rhs

compileLambdasInner (SCall _ args) 
    = concat <$> mapM compileExprLambdas args

compileLambdasInner (SIf expr body) = do
    exprs <- compileExprLambdas expr
    inners <- concat <$> mapM compileLambdasInner body
    return $ exprs <> inners

compileLambdasInner (SIfElse expr bodyIf bodyElse) = do
    exprs <- compileExprLambdas expr
    innersIf <- concat <$> mapM compileLambdasInner bodyIf
    innersElse <- concat <$> mapM compileLambdasInner bodyElse
    return $ exprs <> innersIf <> innersElse

compileLambdasInner (SWhile expr body) = do
    exprs <- compileExprLambdas expr
    inners <- concat <$> mapM compileLambdasInner body
    return $ exprs <> inners

compileLambdasInner (SReturn expr) = compileExprLambdas expr

compileLambdasInner _ = pure []

-- | Compiles a Lambda Expression 
compileExprLambdas :: LocatedExpr -> State Int [Procedure]
compileExprLambdas (LocatedExpr _ (EBinOp _ lhs rhs)) = do
    ls <- compileExprLambdas lhs
    rs <- compileExprLambdas rhs
    return $ ls <> rs

compileExprLambdas (LocatedExpr _ (EUnOp _ inner)) 
    = compileExprLambdas inner

compileExprLambdas (LocatedExpr _ (EFunc _ args)) 
    = concat <$> mapM compileExprLambdas args

compileExprLambdas (LocatedExpr pos (ELambda params retType varDecls body)) = do
    let (LocatedTypeName _ retTypeInner) = retType

    current <- get
    put (current + 1)
    let header = ProcHeader (Ident pos (lambdaLabel current)) params

    return $ pure $ Procedure pos header retTypeInner varDecls body

compileExprLambdas _ = pure []

-----------------------------------
-- VTable Implementation 
-----------------------------------

-- | Generates the vtable instructions for the given symbol table. This works by assigning an index
--   to every procedure, and performing a linear scan on a dedicated register to search for the
--   appropriate procedure.
generateVtable :: RootTable -> [Text]
generateVtable table = mconcat

    [ [ vTableLabel <> ":" ]
    , mconcat branches
    , map addIndent (ozBranch "__vtable_end")
    , mconcat labels
    , [ "__vtable_end:" ]
    , map addIndent $ mconcat
        [ ozWriteString "invalid virtual pointer: "
        , ozReadVPtr (Register 0)
        , ozWriteInt (Register 0)
        , ozWriteString "\\n(exiting)"
        , [ "halt" ] ] ]
    where
        (branches, labels) = unzip $ map extractVtableData (Map.toAscList (rootVtable table))
        extractVtableData (index, (_, locals)) = (branch, target)
        
            where
                procName = localProcName locals
                label  = "__vptr_" <> procName
                branch = map addIndent (ozCmpBranchVPtr index label)
                target = mconcat
                    [ [ label <> ":" ]
                    , map addIndent (ozCall (makeProcLabel procName))
                    , [ addIndent "return" ] ]



