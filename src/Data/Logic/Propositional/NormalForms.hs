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
    toNNF' expr@(Variable _)                    = expr
    toNNF' expr@(Negation (Variable _))         = expr
    toNNF' (Negation (Negation expr))           = toNNF expr
    
    toNNF' (Conjunction exp1 exp2)              = toNNF exp1 `conj` toNNF exp2
    toNNF' (Negation (Conjunction exp1 exp2))   = toNNF $ neg exp1 `disj` neg exp2
    
    toNNF' (Disjunction exp1 exp2)              = toNNF exp1 `disj` toNNF exp2
    toNNF' (Negation (Disjunction exp1 exp2))   = toNNF $ neg exp1 `conj` neg exp2
    
    toNNF' (Conditional exp1 exp2)              = toNNF $ neg exp1 `disj` exp2
    toNNF' (Negation (Conditional exp1 exp2))   = toNNF $ exp1 `conj` neg exp2
    
    toNNF' (Biconditional exp1 exp2)            = let a = exp1 `conj` exp2
                                                      b = neg exp1 `conj` neg exp2
                                                  in toNNF $ a `disj` b
    toNNF' (Negation (Biconditional exp1 exp2)) = let a = exp1 `disj` exp2
                                                      b = neg exp1 `disj` neg exp2
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
    toCNF' (Conjunction exp1 exp2) = toCNF' exp1 `conj` toCNF' exp2
    toCNF' (Disjunction exp1 exp2) = toCNF' exp1 `dist` toCNF' exp2
    toCNF' expr                    = expr
    
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
    toDNF' (Conjunction exp1 exp2) = toDNF' exp1 `dist` toDNF' exp2
    toDNF' (Disjunction exp1 exp2) = toDNF' exp1 `disj` toDNF' exp2
    toDNF' expr                    = expr
    
    dist :: Expr -> Expr -> Expr
    dist (Disjunction e11 e12) e2 = (e11 `dist` e2) `disj` (e12 `dist` e2)
    dist e1 (Disjunction e21 e22) = (e1 `dist` e21) `disj` (e1 `dist` e22)
    dist e1 e2                    = e1 `conj` e2

-- | Performs some simplifications of expressions. The function removes
-- contradictory disjuncts from disjunctions and tautologous conjuncts from
-- conjunctions. When one disjunct implies the other, the disjunction is
-- replaced by the weaker disjunct. Conversely, when one conjunct implies the
-- other the conjunction is replaced by the stronger conjunct. Instances of
-- double negation are eliminated.
--
-- Conditionals and biconditionals are unmodified, but as 'simplify' is
-- generally intended as an internal function for use in the 'toCNF' and 'toDNF'
-- functions this should be considered unproblematic.
simplify :: Expr -> Expr
simplify (Disjunction e1 e2) | isContradiction e1 = simplify e2
                             | isContradiction e2 = simplify e1
                             | implies [e1] e2    = simplify e2
                             | implies [e2] e1    = simplify e1
                             | otherwise          =
                                 simplify e1 `disj` simplify e2
simplify (Conjunction e1 e2) | isTautology e1     = simplify e2
                             | isTautology e2     = simplify e1
                             | implies [e1] e2    = simplify e1
                             | implies [e2] e1    = simplify e2
                             | otherwise          =
                                 simplify e1 `conj` simplify e2
simplify (Negation (Negation e))                  = simplify e
simplify e                                        = e

neg :: Expr -> Expr
neg = Negation

disj :: Expr -> Expr -> Expr
disj = Disjunction

conj :: Expr -> Expr -> Expr
conj = Conjunction
