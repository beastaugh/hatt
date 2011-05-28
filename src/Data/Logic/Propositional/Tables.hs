{-# OPTIONS_HADDOCK hide #-}

module Data.Logic.Propositional.Tables
    ( truthTable
    , truthTableP
    ) where

import Data.Logic.Propositional.Core

import Data.Map (fold)
import Text.PrettyPrint.ANSI.Leijen (green, text, red)

-- | The 'truthTable' function produces a truth table for the given expression.
truthTable :: Expr -> String
truthTable = truthTableP show

-- | The 'truthTableP' is a configurable version of 'truthTable' which allows a
-- printer function to be selected, so for example one can print ASCII truth
-- tables by passing 'showAscii' to 'truthTableP' instead of 'show'.
truthTableP :: (Expr -> String) -> Expr -> String
truthTableP printer expr = unlines [header, separator, body]
  where
    header    = unwords vs ++ " | " ++ printer expr
    body      = init . unlines $ map (showAssignment expr) as
    separator = concat $ replicate sepLength "-"
    sepLength = length vs * 2 + length (printer expr) + 2
    as        = assignments expr
    vs        = variables   expr

showAssignment :: Expr -> Mapping -> String
showAssignment expr a = showVarValues ++ " | " ++ showExprValue
  where
    showVarValues = unwords $ fold ((:) . showBool) [] a
    showExprValue = showBool $ interpret expr a

showBool :: Bool -> String
showBool True  = show . green . text $ "T"
showBool False = show . red . text $ "F"
