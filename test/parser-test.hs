module Main (main) where

import Data.Logic.Propositional

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit.Base

main :: IO ()
main = defaultMain tests

assertParsed :: String -> Expr -> TF.Test
assertParsed toParse expected = testCase ("parse: " ++ toParse) $ either
    (assertFailure . show)
    (assertEqual "parser failed" expected)
    (parseExpr "" toParse)

tests :: [TF.Test]
tests = [ testGroup "Parser tests"
            [ assertParsed "p" p
            , assertParsed "    p" p
            , assertParsed "p    " p
            , assertParsed "    p    " p
            , assertParsed "(p)" p
            , assertParsed "((p))" p
            , assertParsed "~p" (neg p)
            , assertParsed "(~p)" (neg p)
            , assertParsed "((~p))" (neg p)
            , assertParsed " (     (p ) )  " p
            , assertParsed " (  ~   (p ) )  " (neg p)
            , assertParsed "p & q" (p `conj` q)
            , assertParsed "(p & q)" (p `conj` q)
            , assertParsed "p & ~q" (p `conj` neg q)
            , assertParsed "p & (~q)" (p `conj` neg q)
            , assertParsed "(p & (~q & ~r))" (p `conj` (neg q `conj` neg r))
            , assertParsed "(p <-> q)" (p `iff` q)
            , assertParsed "p -> (q & ~r)" (p `cond` (q `conj` neg r))
            , assertParsed "p | q | ~r" (p `disj` (q `disj` neg r))
            , assertParsed "p & q | r" ((p `conj` q) `disj` r)
            , assertParsed "~p | q -> ~r" ((neg p `disj` q) `cond` neg r)
            , assertParsed "p <-> ~q -> r" (p `iff` (neg q `cond` r))
            ]
        , testGroup "QuickCheck Data.Logic.Propositional"
            [ testProperty "UnserialisedExprsParse" propUnserialisedExprsParse
            ]
        ]
  where
    neg  = Negation
    conj = Conjunction
    disj = Disjunction
    cond = Conditional
    iff  = Biconditional
    var  = Variable . Var
    p    = var 'p'
    q    = var 'q'
    r    = var 'r'

propUnserialisedExprsParse :: Expr -> Bool
propUnserialisedExprsParse input = case parseExpr "" $ showAscii input of
                                     Right output -> input == output
                                     Left  _      -> False
