{-# LANGUAGE DeriveDataTypeable #-}

module Main
    ( main
    ) where

import Data.Logic.Propositional

import Data.Char (isSpace, toLower)
import System.Console.CmdArgs
import System.IO

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
          let expStr    = evaluate opts
              interMode = interactive opts
              evalMode  = (not . null) expStr
              printer   = if pretty opts then show else showAscii
          
          -- If the --evaluate flag is passed with an expression, print the
          -- truth table for that expression.
          if evalMode
            then putStr $ eval printer expStr
            else return ()
          
          -- If the --evaluate flag is passed with an expression and
          -- interactive mode is NOT explicitly requested, terminate the
          -- program; otherwise, enter interactive mode.
          if evalMode && not interMode
            then return ()
            else putStrLn "Entering interactive mode..." >> repl

repl :: IO ()
repl = do putStr "> "
          hFlush stdout
          cmd <- getLine
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
