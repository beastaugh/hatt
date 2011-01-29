{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Logic.Propositional.Parser
    ( parseExpr
    ) where

import Logic.Propositional.Core (Expr (..))

import Text.ParserCombinators.Parsec
       (char, choice, eof, oneOf, parse, spaces, string)

import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Pos (SourceName)
import Text.ParserCombinators.Parsec.Prim (GenParser)

parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = parse statement

statement :: GenParser Char st Expr
statement = do spaces
               x <- expr
               spaces
               eof
               return x

expr :: GenParser Char st Expr
expr = choice [binaryP, negation, constant]

constant :: GenParser Char st Expr
constant = do c <- oneOf ['A'..'Z']
              return $ Variable [c]

negation :: GenParser Char st Expr
negation = do char '~'
              x <- expr
              return $ Negation x

binaryP :: GenParser Char st Expr
binaryP = do char '('
             x <- binary
             char ')'
             return x

binary :: GenParser Char st Expr
binary = do x1 <- expr
            spaces
            s  <- choice [string "&", string "|", string "->", string "<->"]
            spaces
            x2 <- expr
            return $ connective s x1 x2
  where
    connective "&"   = Conjunction
    connective "|"   = Disjunction
    connective "->"  = Conditional
    connective "<->" = Biconditional
    connective _     = undefined
