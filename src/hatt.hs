{-# LANGUAGE DeriveDataTypeable #-}

module Main
    ( main
    ) where

import Data.Logic.Propositional

import Data.Char (isSpace, toLower)
import System.Console.CmdArgs

data HattOpts = HattOpts
    { evaluate    :: String
    , interactive :: Bool
    , pretty      :: Bool
    } deriving (Show, Data, Typeable)

data Command = Exit
             | Help
             | Eval Expr
             | Error String

hattOpts :: HattOpts
hattOpts = HattOpts
  { evaluate    = "" &= typ  "EXPRESSION"
                     &= help "Print the truth table for the given expression"
  , interactive = False &= help "Enter interactive mode"
  , pretty      = False &= help "Use Unicode logic symbols"
  } &= summary "Hatt 0.2, (c) Benedict Eastaugh 2011"
    &= program "hatt"

main :: IO ()
main = do opts <- cmdArgs hattOpts
          case evaluate opts of
            ""   -> putStrLn "Try using the --evaluate[=EXPRESSION] flag"
            expr -> putStr $
                    eval (if pretty opts then show else showAscii) expr
          case interactive opts of
            True -> putStrLn "Entering interactive mode..." >> repl
            _    -> return ()

repl :: IO ()
repl = do cmd <- getLine
          case parseCommand cmd of
            Exit        -> return ()
            Help        -> putStr   replHelpText       >> repl
            (Eval expr) -> putStr   (truthTable expr)  >> repl
            (Error err) -> putStrLn ("Error: " ++ err) >> repl

eval :: (Expr -> String) -> String -> String
eval p str = case parseExpr "" str of
               Left  err  -> "Parse error at " ++ show err ++ "\n"
               Right expr -> truthTableP p expr

parseCommand :: String -> Command
parseCommand s = case map toLower cmd of
                   "exit" -> Exit
                   "help" -> Help
                   "eval" -> eval_ rest
                   ""     -> Error "no command entered"
                   other  -> Error $ "unknown command " ++ other
  where
    (cmd, rest) = splitOnFirstSpace . dropWhile isSpace $ s
    eval_ str   = case parseExpr "" str of
                    Left  err  -> Error $ "parse error at " ++ show err
                    Right expr -> Eval expr

replHelpText :: String
replHelpText = unlines
  [ "The following commands are available in Hatt's interactive mode:"
  , ""
  , "eval [EXPRESSION]"
  , "  Evaluate an expression. For example, if you enter \"eval (A -> B)\","
  , "  Hatt will print the truth table for the expression \"(A -> B)\"."
  , ""
  , "help"
  , "  Print this help text."
  , ""
  , "exit"
  , "  Quit the program."
  ]

splitOnFirstSpace :: String -> (String, String)
splitOnFirstSpace str = case words str of
                          []             -> ("", "")
                          (first : rest) -> (first, unwords rest)
