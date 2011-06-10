{-# OPTIONS_HADDOCK hide #-}

module Data.Logic.Propositional.Tables
    ( Printer
    , colourBool
    , showBool
    , truthTable
    , truthTableP
    ) where

import Data.Logic.Propositional.Core

import Data.Map (fold)
import Text.PrettyPrint.ANSI.Leijen (green, text, red)

type Printer = (Expr -> String, Bool -> String)

-- | The 'truthTable' function produces a truth table for the given expression.
truthTable :: Expr -> String
truthTable = truthTableP (show, colourBool)

-- | The 'truthTableP' is a configurable version of 'truthTable' which allows a
-- printer function to be selected, so for example one can print ASCII truth
-- tables by passing 'showAscii' to 'truthTableP' instead of 'show'.
truthTableP :: Printer -> Expr -> String
truthTableP (expPrinter, boolPrinter) expr = unlines [header, separator, body]
  where
    header    = unwords vs ++ " | " ++ expPrinter expr
    body      = init . unlines $ map (showAssignment boolPrinter expr) as
    separator = concat $ replicate sepLength "-"
    sepLength = length vs * 2 + length (expPrinter expr) + 2
    as        = assignments expr
    vs        = variables   expr

showAssignment :: (Bool -> String) -> Expr -> Mapping -> String
showAssignment printer expr a = showVarValues ++ " | " ++ showExprValue
  where
    showVarValues = unwords $ fold ((:) . printer) [] a
    showExprValue = printer $ interpret expr a

showBool :: Bool -> String
showBool True  = "T"
showBool False = "F"

colourBool :: Bool -> String
colourBool True  = show . green . text $ "T"
colourBool False = show . red . text $ "F"
