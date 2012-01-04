{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Data.Logic.Propositional
import Data.Logic.Propositional.Tables

import Control.Monad (when, unless)
import Data.Char (isSpace, toLower)
import System.Console.CmdArgs
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStr, outputStrLn)

data Command = Exit
             | Help
             | Pretty
             | Coloured
             | Eval Expr
             | Error String

data ProgramMode = ProgramMode
  { evaluate    :: String
  , interactive :: Bool
  , pretty      :: Bool
  , coloured    :: Bool
  } deriving (Show, Data, Typeable)

programMode :: ProgramMode
programMode = ProgramMode
  { evaluate    = "" &= typ  "EXPRESSION"
                     &= help "Print the truth table for the given expression"
  , interactive = False &= help "Enter interactive mode"
  , pretty      = False &= help "Use Unicode logic symbols"
  , coloured    = False &= help "Use colour-coded symbols"
  } &= summary "Hatt 1.4.0.2, (c) Benedict Eastaugh 2012"
    &= program "hatt"

main :: IO ()
main = do opts <- cmdArgs programMode
          let expStr    = evaluate opts
              interMode = interactive opts
              evalMode  = (not . null) expStr
              printer   = selectPrinter opts
          
          -- If the --evaluate flag is passed with an expression, print the
          -- truth table for that expression.
          when evalMode $ putStr (eval printer expStr)
          
          -- Unless the --evaluate flag is passed with an expression and
          -- interactive mode is NOT explicitly requested, terminate the
          -- program; otherwise, enter interactive mode.
          unless (evalMode && not interMode) $
              putStrLn replIntroText
              >> runInputT defaultSettings (repl opts)

repl :: ProgramMode -> InputT IO ()
repl mode = do
    minput <- getInputLine "> "
    case minput of
      Nothing  -> return ()
      Just cmd -> case parseCommand cmd of
        Exit        -> return ()
        Help        -> outputStr (replHelpText printer)
                       >> repl mode
        Pretty      -> outputStrLn ppMessage
                       >> repl (mode {pretty = not isPretty})
        Coloured    -> outputStrLn cpMessage
                       >> repl (mode {coloured = not isColoured})
        (Eval expr) -> outputStr (truthTableP printer expr)
                       >> repl mode
        (Error err) -> outputStrLn ("Error: " ++ err)
                       >> repl mode
  where
    printer    = selectPrinter mode
    isPretty   = pretty mode
    isColoured = coloured mode
    ppMessage  = (if isPretty then "Dis" else "En") ++ "abling pretty-printing."
    cpMessage  = (if isColoured then "Dis" else "En") ++ "abling colour-coding."

eval :: Printer -> String -> String
eval p str = case parseExpr "" str of
               Left  err  -> "Parse error at " ++ show err ++ "\n"
               Right expr -> truthTableP p expr

parseCommand :: String -> Command
parseCommand input = case cmd . words . dropWhile isSpace $ input of
                       ""       -> Error "you must enter an expression or a command."
                       "exit"   -> Exit
                       "help"   -> Help
                       "pretty" -> Pretty
                       "colour" -> Coloured
                       _        -> eval_ input
  where
    cmd []    = ""
    cmd ws    = map toLower . head $ ws
    eval_ str = case parseExpr "" str of
                  Left  err  -> Error $ "parse error at " ++ show err
                  Right expr -> Eval expr

replIntroText :: String
replIntroText = unwords
  [ "Entering interactive mode."
  , "Type `help` if you don't know what to do!"
  ]

replHelpText :: Printer -> String
replHelpText printer = unlines
  [ "Hatt's interactive mode has several commands."
  , ""
  , "help"
  , "  Print this help text."
  , ""
  , "pretty"
  , "  Pretty-print expressions using Unicode logic symbols. Only employ this"
  , "  option if your console is Unicode-aware. If pretty-printing is already"
  , "  enabled, using this command will disable it."
  , ""
  , "colour"
  , "  Colour truth values: green for true, red for false. This feature needs"
  , "  your console to support ANSI colour codes. If coloured mode is already"
  , "  enabled, this command will disable it."
  , ""
  , "exit"
  , "  Quit the program."
  , ""
  , "If you don't type in a command, the program will assume you're writing a"
  , "logical expression to be evaluated and attempt to parse it."
  , ""
  , "For example, if you enter \"(A -> B)\" at the prompt, Hatt will print the"
  , "truth table for that expression. Here's an example console session."
  , ""
  , "    > A | B"
  , indentBy 4 $ truthTableP printer (Disjunction (Variable "A") (Variable "B"))
 ++ "> P -> (Q & R)\n"
 ++ truthTableP printer (Conditional
                          (Variable "P")
                          (Conjunction (Variable "Q") (Variable "R")))
  , "If none of this makes any sense, try reading the README file."
  ]

selectPrinter :: ProgramMode -> Printer
selectPrinter m = let expPrinter   = if pretty m then show else showAscii
                      tablePrinter = if coloured m then colourBool else showBool
                  in (expPrinter, tablePrinter)

indentBy :: Int -> String -> String
indentBy n = unlines . map (replicate n ' ' ++) . lines
