{-# LANGUAGE DeriveDataTypeable #-}

module Main
    ( main
    ) where

import Data.Logic.Propositional

import Data.Char (isSpace, toLower)
import System.Console.CmdArgs
import System.IO

data Command = Exit
             | Help
             | Pretty
             | Eval Expr
             | Error String

data ProgramMode = ProgramMode
  { evaluate    :: String
  , interactive :: Bool
  , pretty      :: Bool
  } deriving (Show, Data, Typeable)

programMode :: ProgramMode
programMode = ProgramMode
  { evaluate    = "" &= typ  "EXPRESSION"
                     &= help "Print the truth table for the given expression"
  , interactive = False &= help "Enter interactive mode"
  , pretty      = False &= help "Use Unicode logic symbols"
  } &= summary "Hatt 1.0, (c) Benedict Eastaugh 2011"
    &= program "hatt"

main :: IO ()
main = do opts <- cmdArgs programMode
          let expStr    = evaluate opts
              interMode = interactive opts
              evalMode  = (not . null) expStr
              printer   = selectPrinter opts
          
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
            else putStrLn replIntroText >> repl opts

repl :: ProgramMode -> IO ()
repl mode = do putStr "> "
               hFlush stdout
               cmd <- getLine
               case parseCommand cmd of
                 Exit        -> return ()
                 Help        -> putStr replHelpText
                                >> repl mode
                 Pretty      -> putStrLn ppMessage
                                >> repl (mode {pretty = not isPretty})
                 (Eval expr) -> putStr (truthTableP printer expr)
                                >> repl mode
                 (Error err) -> putStrLn ("Error: " ++ err)
                                >> repl mode
  where
    printer   = selectPrinter mode
    isPretty  = pretty mode
    ppMessage = (if isPretty then "Dis" else "En") ++ "abling pretty-printing."

eval :: (Expr -> String) -> String -> String
eval p str = case parseExpr "" str of
               Left  err  -> "Parse error at " ++ show err ++ "\n"
               Right expr -> truthTableP p expr

parseCommand :: String -> Command
parseCommand input = case cmd . words . dropWhile isSpace $ input of
                       ""       -> Error "you must enter an expression or a command."
                       "exit"   -> Exit
                       "help"   -> Help
                       "pretty" -> Pretty
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

replHelpText :: String
replHelpText = unlines
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
  , "exit"
  , "  Quit the program."
  , ""
  , "If you don't type in a command, the program will assume you're writing a"
  , "logical expression to be evaluated and attempt to parse it."
  , ""
  , "For example, if you enter \"(A -> B)\" at the prompt, Hatt will print the"
  , "truth table for that expression. Here's an example console session."
  , ""
  , "    > (A | B)"
  , "    A B | (A ∨ B)"
  , "    -------------"
  , "    T T | T"
  , "    T F | T"
  , "    F T | T"
  , "    F F | F"
  , "    > foobar"
  , "    Error: parse error at (line 1, column 1):"
  , "    unexpected \"f\""
  , "    expecting white space, \"(\" or \"~\""
  , "   > exit"
  , ""
  , "If none of this makes any sense, try reading the README file."
  ]

selectPrinter :: ProgramMode -> (Expr -> String)
selectPrinter m = if pretty m then show else showAscii
