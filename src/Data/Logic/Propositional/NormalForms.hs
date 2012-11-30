-- | The functions exposed by this module convert expressions of type 'Expr'
-- into different normal forms: negation normal form via 'toNNF', conjunctive
-- normal form via 'toCNF' and disjunctive normal form via 'toDNF'. All these
-- functions are total.
module Data.Logic.Propositional.NormalForms
    ( toNNF
    , toCNF
    , toDNF
    , simplify
    ) where

import Data.Logic.Propositional.Core

-- | The 'toNNF' function converts expressions to negation normal form. This
-- function is total: it's defined for all expressions, not just those which
-- only use negation, conjunction and disjunction, although all expressions in
-- negation normal form do in fact only use those connectives.
--
-- The conversion is carried out by replacing any condtitionals or
-- biconditionals with equivalent expressions using only negation, conjunction
-- and disjunction. Then de Morgan's laws are applied to convert negated
-- conjunctions and disjunctions into the conjunction or disjunction of the
-- negation of their conjuncts: @¬(φ ∧ ψ)@ is converted to @(¬φ ∨ ¬ψ)@
-- while @¬(φ ∨ ψ)@ becomes @(¬φ ∧ ¬ψ)@.
toNNF :: Expr -> Expr
toNNF = toNNF' . simplify
  where
    toNNF' expr@(Variable _)                = expr
    toNNF' expr@(Negation (Variable _))     = expr
    toNNF' (Negation (Negation expr))       = toNNF expr
    
    toNNF' (Conjunction e1 e2)              = toNNF e1 `conj` toNNF e2
    toNNF' (Negation (Conjunction e1 e2))   = toNNF $ neg e1 `disj` neg e2
    
    toNNF' (Disjunction e1 e2)              = toNNF e1 `disj` toNNF e2
    toNNF' (Negation (Disjunction e1 e2))   = toNNF $ neg e1 `conj` neg e2
    
    toNNF' (Conditional e1 e2)              = toNNF $ neg e1 `disj` e2
    toNNF' (Negation (Conditional e1 e2))   = toNNF $ e1 `conj` neg e2
    
    toNNF' (Biconditional e1 e2)            = let a = e1 `conj` e2
                                                  b = neg e1 `conj` neg e2
                                              in toNNF $ a `disj` b
    toNNF' (Negation (Biconditional e1 e2)) = let a = e1 `disj` e2
                                                  b = neg e1 `disj` neg e2
                                              in toNNF $ a `conj` b

-- | The 'toCNF' function converts expressions to conjunctive normal form: a
-- conjunction of clauses, where a clause is a disjunction of literals
-- (variables and negated variables).
--
-- The conversion is carried out by first converting the expression into
-- negation normal form, and then applying the distributive law.
--
-- Because it first applies 'toNNF', it is a total function and can handle
-- expressions which include conditionals and biconditionals.
toCNF :: Expr -> Expr
toCNF = simplify . toCNF' . toNNF
  where
    toCNF' :: Expr -> Expr
    toCNF' (Conjunction e1 e2) = toCNF' e1 `conj` toCNF' e2
    toCNF' (Disjunction e1 e2) = toCNF' e1 `dist` toCNF' e2
    toCNF' expr                = expr
    
    dist :: Expr -> Expr -> Expr
    dist (Conjunction e11 e12) e2 = (e11 `dist` e2) `conj` (e12 `dist` e2)
    dist e1 (Conjunction e21 e22) = (e1 `dist` e21) `conj` (e1 `dist` e22)
    dist e1 e2                    = e1 `disj` e2

-- | The 'toDNF' function converts expressions to disjunctive normal form: a
-- disjunction of clauses, where a clause is a conjunction of literals
-- (variables and negated variables).
--
-- The conversion is carried out by first converting the expression into
-- negation normal form, and then applying the distributive law.
--
-- Because it first applies 'toNNF', it is a total function and can handle
-- expressions which include conditionals and biconditionals.
toDNF :: Expr -> Expr
toDNF = simplify. toDNF' . toNNF
  where
    toDNF' :: Expr -> Expr
    toDNF' (Conjunction e1 e2) = toDNF' e1 `dist` toDNF' e2
    toDNF' (Disjunction e1 e2) = toDNF' e1 `disj` toDNF' e2
    toDNF' expr                    = expr
    
    dist :: Expr -> Expr -> Expr
    dist (Disjunction e11 e12) e2 = (e11 `dist` e2) `disj` (e12 `dist` e2)
    dist e1 (Disjunction e21 e22) = (e1 `dist` e21) `disj` (e1 `dist` e22)
    dist e1 e2                    = e1 `conj` e2

-- | Performs some simplifications of expressions. When one disjunct implies the
-- other, the disjunction is reduced to the weaker disjunct. As a special case,
-- if one disjunct is a contradiction then the disjunction is reduced to the
-- other disjunct. Conversely, when one conjunct implies the other the
-- conjunction is reduced to the stronger conjunct. As a special case, when one
-- conjuct is a tautology then the conjunction is reduced to the other conjunct.
-- Instances of double negation are also reduced away.
--
-- Conditionals and biconditionals are left unmodified, but as 'simplify' is
-- primarily intended as an internal function for use in the 'toCNF' and 'toDNF'
-- functions this should be considered unproblematic.
simplify :: Expr -> Expr
simplify (Disjunction e1 e2) | implies [e1] e2 = simplify e2
                             | implies [e2] e1 = simplify e1
                             | otherwise       = simplify e1 `disj` simplify e2
simplify (Conjunction e1 e2) | implies [e1] e2 = simplify e1
                             | implies [e2] e1 = simplify e2
                             | otherwise       = simplify e1 `conj` simplify e2
simplify (Negation (Negation e))               = simplify e
simplify e                                     = e

neg :: Expr -> Expr
neg = Negation

disj :: Expr -> Expr -> Expr
disj = Disjunction

conj :: Expr -> Expr -> Expr
conj = Conjunction
