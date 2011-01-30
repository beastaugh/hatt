-- | The "Logic.Propositional" module provides a set of functions for parsing,
-- manipulating and generating truth tables for expressions in classical
-- propositional logic.
--
-- The core of the API is the 'Expr' data type, which has constructors for all
-- the usual expression forms: variables, standing for atomic propositions;
-- negation, the only unary connective; and the binary connectives of
-- conjunction, disjunction, material implication and logical equivalence.
module Logic.Propositional
    ( Expr (..)
    , Mapping
    
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
