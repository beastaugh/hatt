module Main (main) where

import Data.Logic.Propositional

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
      [ testGroup "QuickCheck Data.Logic.Propositional.NormalForms"
          [ testProperty "SelfEquiv" propSelfEquiv
          , testProperty "NNFEquiv"  propNNFEquiv
          , testProperty "CNFEquiv"  propCNFEquiv
          , testProperty "DNFEquiv"  propDNFEquiv
          ]
      ]

propSelfEquiv :: Expr -> Bool
propSelfEquiv expr = expr `equivalent` expr

propNNFEquiv :: Expr -> Bool
propNNFEquiv expr = expr `equivalent` toNNF expr

propCNFEquiv :: Expr -> Bool
propCNFEquiv expr = expr `equivalent` toCNF expr

propDNFEquiv :: Expr -> Bool
propDNFEquiv expr = expr `equivalent` toDNF expr
