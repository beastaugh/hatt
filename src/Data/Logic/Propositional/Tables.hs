-- | This module contains the truth table generating functionality of Hatt. The
-- core function it exports is 'truthTable' which prints the truth table of the
-- given expression. 'truthTableP' is a configurable version which allows one to
-- select how to print expressions and truth values. This gives one the option
-- of, for example, colouring outputs and changing the symbols used to represent
-- the logical connectives.
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
    header    = (unwords . map show) vs ++ " | " ++ expPrinter expr
    body      = init . unlines $ map (showAssignment boolPrinter expr) as
    separator = concat $ replicate sepLength "-"
    sepLength = length vs * 2 + length (expPrinter expr) + 2
    as        = assignments [expr]
    vs        = variables   expr

showAssignment :: (Bool -> String) -> Expr -> Mapping -> String
showAssignment printer expr a = showVarValues ++ " | " ++ showExprValue
  where
    showVarValues = unwords $ fold ((:) . printer) [] a
    showExprValue = printer $ interpret expr a

-- | Prints @T@ for 'True' and @F@ for 'False'.
showBool :: Bool -> String
showBool True  = "T"
showBool False = "F"

-- | Prints a green @T@ for 'True' and a red @F@ for 'False'. This is used when
-- producing a string representation of a truth table with 'truthTable'. It can
-- also be used as (as the second component of a 'Printer' pair) as an argument
-- to the configurable 'truthTableP' function.
colourBool :: Bool -> String
colourBool True  = show . green . text $ "T"
colourBool False = show . red . text $ "F"
