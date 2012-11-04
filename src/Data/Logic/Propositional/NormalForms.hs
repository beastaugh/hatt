{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Logic.Propositional.NormalForms
    ( toNNF
    ) where

import Data.Logic.Propositional.Core

toNNF :: Expr -> Expr
toNNF expr@(Variable _)                    = expr
toNNF expr@(Negation (Variable _))         = expr
toNNF (Negation (Negation expr))           = expr

toNNF (Conjunction exp1 exp2)              = toNNF exp1 `conj` toNNF exp2
toNNF (Negation (Conjunction exp1 exp2))   = toNNF $ neg exp1 `disj` neg exp2

toNNF (Disjunction exp1 exp2)              = toNNF exp1 `disj` toNNF exp2
toNNF (Negation (Disjunction exp1 exp2))   = toNNF $ neg exp1 `conj` neg exp2

toNNF (Conditional exp1 exp2)              = toNNF $ neg exp1 `disj` exp2
toNNF (Negation (Conditional exp1 exp2))   = toNNF $ exp1 `conj` neg exp2

toNNF (Biconditional exp1 exp2)            = let a = exp1 `conj` exp2
                                                 b = neg exp1 `conj` neg exp2
                                             in toNNF $ a `disj` b
toNNF (Negation (Biconditional exp1 exp2)) = let a = exp1 `disj` exp2
                                                 b = neg exp1 `disj` neg exp2
                                             in toNNF $ a `conj` b

neg :: Expr -> Expr
neg = Negation

disj :: Expr -> Expr -> Expr
disj = Disjunction

conj :: Expr -> Expr -> Expr
conj = Conjunction
