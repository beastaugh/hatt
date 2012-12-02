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
          , testProperty "AnyImpliesTautology" propAnyImpliesTautology
          , testProperty "ConjImpliesConjuncts" propConjImpliesConjuncts
          , testProperty "EquivsImplyEachOther" propEquivsImplyEachOther
          ]
       , testGroup "QuickCheck Data.Logic.Propositional.NormalForms"
          [ testProperty "NNFNoConds" propNNFNoConds
          , testProperty "NNFNegationInside" propNNFNegationInside
          , testProperty "NNFEquiv" propNNFEquiv
          , testProperty "CNFEquiv" propCNFEquiv
          , testProperty "DNFEquiv" propDNFEquiv
          , testProperty "CNF_Equiv" propCNF_Equiv
          , testProperty "DNF_Equiv" propDNF_Equiv
          , testProperty "CNFConvsEquiv" propCNFConvsEquiv
          , testProperty "DNFConvsEquiv" propDNFConvsEquiv
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

propNNFNegationInside :: Expr -> Bool
propNNFNegationInside = negInside . toNNF
  where
    negInside (Variable _)            = True
    negInside (Negation (Variable _)) = True
    negInside (Negation _)            = False
    negInside (Conjunction a b)       = negInside a && negInside b
    negInside (Disjunction a b)       = negInside a && negInside b
    negInside (Conditional _ _)       = error "No conditionals in NNF"
    negInside (Biconditional _ _)     = error "No biconditionals in NNF"

propNNFEquiv :: Expr -> Bool
propNNFEquiv expr = expr `equivalent` toNNF expr

propCNFEquiv :: Expr -> Bool
propCNFEquiv expr = expr `equivalent` toCNF expr

propDNFEquiv :: Expr -> Bool
propDNFEquiv expr = expr `equivalent` toDNF expr

propCNF_Equiv :: Expr -> Bool
propCNF_Equiv expr = expr `equivalent` toCNF_ expr

propDNF_Equiv :: Expr -> Bool
propDNF_Equiv expr = expr `equivalent` toDNF_ expr

propCNFConvsEquiv :: Expr -> Bool
propCNFConvsEquiv expr = toCNF expr `equivalent` toCNF_ expr

propDNFConvsEquiv :: Expr -> Bool
propDNFConvsEquiv expr = toDNF expr `equivalent` toDNF_ expr

propSimpEquiv :: Expr -> Bool
propSimpEquiv expr = expr `equivalent` simplify expr

propEquivsImplyEachOther :: Expr -> Expr -> Bool
propEquivsImplyEachOther e1 e2 | equivalent e1 e2 = [e1] `implies` e2
                                                 && [e2] `implies` e1
                               | otherwise        = True

propContradictionImpliesAll :: Expr -> Expr -> Bool
propContradictionImpliesAll e1 e2 | isContradiction e1 = [e1] `implies` e2
                                  | otherwise          = True

propAnyImpliesTautology :: [Expr] -> Expr -> Bool
propAnyImpliesTautology es e | isTautology e = es `implies` e
                             | otherwise     = True

propConjImpliesConjuncts :: Expr -> Bool
propConjImpliesConjuncts e@(Conjunction a b) = [e] `implies` a
                                            && [e] `implies` b
propConjImpliesConjuncts _                   = True
