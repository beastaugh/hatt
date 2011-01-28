module Main (main) where

import Logic.Propositional

import Data.Either ( Either (..) )

main = putStr $ case parseExpr "" "(A -> ~B)" of
                  Left  err  -> "parse error at " ++ show err
                  Right expr -> truthTable expr
