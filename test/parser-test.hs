module Main (main) where

import Data.Logic.Propositional

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit( testCase )
import Test.HUnit.Base

assertParsed :: String -> Expr -> TF.Test
assertParsed toParse expected = testCase ("parse: " ++ toParse) $ either
    (assertFailure . show)
    (assertEqual "parser failed" expected)
    (parseExpr "" toParse)

tests :: [TF.Test]
tests = [ testGroup "Parser tests"
            [ assertParsed "p" p
            , assertParsed "(p)" p
            , assertParsed "((p))" p
            , assertParsed "~p" (neg p)
            , assertParsed "(~p)" (neg p)
            , assertParsed "((~p))" (neg p)
            , assertParsed "p & q" (con p q)
            , assertParsed "(p & q)" (con p q)
            , assertParsed "p & ~q" (con p (neg q))
            , assertParsed "p & (~q)" (con p (neg q))
            , assertParsed "(p & (~q & ~r))" (p `con` (neg q `con` neg r))
            , assertParsed "(p <-> q)" (Biconditional p q)
            , assertParsed "p -> (q & ~r)" (p `Conditional` con q (neg r))
            ]
        ]
  where
    neg = Negation
    con = Conjunction
    p = Variable $ Var 'p'
    q = Variable $ Var 'q'
    r = Variable $ Var 'r'

main :: IO ()
main = defaultMain tests

