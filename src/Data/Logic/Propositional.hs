-- | The "Data.Logic.Propositional" module provides a set of functions for
-- parsing, manipulating and generating truth tables for expressions in
-- classical propositional logic.
--
-- The core of the API is the 'Expr' data type, which has constructors for all
-- the usual expression forms: variables, standing for atomic propositions;
-- negation, the only unary connective; and the binary connectives of
-- conjunction, disjunction, material implication and logical equivalence.
module Data.Logic.Propositional
    ( Expr (..)
    , Var (..)
    , Mapping
    
    , equivalent
    , interpret
    , assignments
    , values
    , variables
    , isContingent
    , isContradiction
    , isTautology
    
    , parseExpr
    
    , show
    , showAscii
    
    , truthTable
    , truthTableP
    ) where

import Data.Logic.Propositional.Core
import Data.Logic.Propositional.Parser
import Data.Logic.Propositional.Tables (truthTable, truthTableP)
