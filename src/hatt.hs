{-# LANGUAGE DeriveDataTypeable #-}

module Main
    ( main
    ) where

import Data.Logic.Propositional

import System.Console.CmdArgs

data HattOpts = HattOpts
    { evaluate :: String
    , pretty   :: Bool
    } deriving (Show, Data, Typeable)

hattOpts :: HattOpts
hattOpts = HattOpts
  { evaluate = ""    &= typ "EXPRESSION"
                     &= help "Print the truth table for the given expression"
  , pretty   = False &= help "Use Unicode logic symbols when printing expressions"
  } &= summary "Hatt 0.1, (c) Benedict Eastaugh 2011"
    &= program "hatt"

main :: IO ()
main = do opts <- cmdArgs hattOpts
          case evaluate opts of
               ""   -> putStrLn "Try using the --evaluate[=EXPRESSION] flag"
               expr -> putStr (eval (if pretty opts then show else showAscii) expr)

eval :: (Expr -> String) -> String -> String
eval p str = case parseExpr "" str of
                  Left  err  -> "parse error at " ++ show err ++ "\n"
                  Right expr -> truthTableP p expr
