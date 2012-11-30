module Main (main) where

import Data.Logic.Propositional
import Data.Logic.Propositional.NormalForms

import Test.Framework as TF (defaultMainWithArgs, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMainWithArgs tests ["-d 10"]

tests :: [TF.Test]
tests =
      [ testGroup "QuickCheck Data.Logic.Propositional"
          [ testProperty "SelfEquiv" propSelfEquiv
          , testProperty "ContradictionImpliesAll" propContradictionImpliesAll
          , testProperty "NullImpliesTautologies" propNullImpliesTautologies
          , testProperty "ConjImpliesConjuncts" propConjImpliesConjuncts
          , testProperty "EquivsImplyEachOther" propEquivsImplyEachOther
          ]
       , testGroup "QuickCheck Data.Logic.Propositional.NormalForms"
          [ testProperty "NNFNoConds" propNNFNoConds
          , testProperty "NNFEquiv" propNNFEquiv
          , testProperty "CNFEquiv" propCNFEquiv
          , testProperty "DNFEquiv" propDNFEquiv
          , testProperty "SimpEquiv" propSimpEquiv
          ]
      ]

propSelfEquiv :: Expr -> Bool
propSelfEquiv expr = expr `equivalent` expr

propNNFNoConds :: Expr -> Bool
propNNFNoConds = noConds . toNNF
  where
    noConds (Variable _)        = True
    noConds (Negation e)        = noConds e
    noConds (Conjunction a b)   = noConds a && noConds b
    noConds (Disjunction a b)   = noConds a && noConds b
    noConds (Conditional _ _)   = False
    noConds (Biconditional _ _) = False

propNNFEquiv :: Expr -> Bool
propNNFEquiv expr = expr `equivalent` toNNF expr

propCNFEquiv :: Expr -> Bool
propCNFEquiv expr = expr `equivalent` toCNF expr

propDNFEquiv :: Expr -> Bool
propDNFEquiv expr = expr `equivalent` toDNF expr

propSimpEquiv :: Expr -> Bool
propSimpEquiv expr = expr `equivalent` simplify expr

propEquivsImplyEachOther :: Expr -> Expr -> Bool
propEquivsImplyEachOther e1 e2 | equivalent e1 e2 = [e1] `implies` e2
                                                 && [e2] `implies` e1
                               | otherwise        = True

propContradictionImpliesAll :: Expr -> Bool
propContradictionImpliesAll expr = [falsum] `implies` expr

propNullImpliesTautologies :: Expr -> Bool
propNullImpliesTautologies expr | isTautology expr = [] `implies` expr
                                | otherwise = True

propConjImpliesConjuncts :: Expr -> Bool
propConjImpliesConjuncts e@(Conjunction a b) = [e] `implies` a
                                            && [e] `implies` b
propConjImpliesConjuncts _                   = True

falsum :: Expr
falsum = let a = Variable $ Var 'a'
         in Conjunction a (Negation a)
