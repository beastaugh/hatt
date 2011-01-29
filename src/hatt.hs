{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Logic.Propositional

import System.Console.CmdArgs
import Data.Either ( Either (..) )

data HattOpts = HattOpts
    { evaluate :: String
    } deriving (Show, Data, Typeable)

hattOpts :: HattOpts
hattOpts = HattOpts
  { evaluate = def &= opt "" &= typ "EXPRESSION"
                   &= help "Print the truth table for the given expression"
  } &= summary "Hatt 0.1, (c) Benedict Eastaugh 2011"
    &= program "hatt"

main :: IO ()
main = do opts <- cmdArgs hattOpts
          case evaluate opts of
               ""   -> putStrLn "Try using the --evaluate[=EXPRESSION] flag"
               expr -> putStr (eval expr)

eval :: String -> String
eval str = case parseExpr "" str of
                Left  err  -> "parse error at " ++ show err ++ "\n"
                Right expr -> truthTable expr
