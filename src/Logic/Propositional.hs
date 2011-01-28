module Logic.Propositional
    ( Expr (..)
    , Mapping (..)
    
    , equivalent
    , interpret
    , assignments
    , isContingent
    , isContradiction
    , isTautology
    , parseExpr
    , show
    , showAscii
    , truthTable
    , variables
    ) where

import Logic.Propositional.Core
import Logic.Propositional.Parser
import Logic.Propositional.Tables
