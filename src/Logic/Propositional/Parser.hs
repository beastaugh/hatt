module Logic.Propositional.Parser
    ( parseExpr
    ) where

import Logic.Propositional.Core ( Expr (..) )

import Text.ParserCombinators.Parsec
    ( Parser (..)
    , char
    , choice
    , eof
    , oneOf
    , parse
    , spaces
    , string
    )

parseExpr = parse statement

statement = do spaces
               x <- expr
               spaces
               eof
               return x

expr = choice [binaryP, negation, constant]

constant = do c <- oneOf ['A'..'Z']
              return $ Variable [c]

negation = do char '~'
              x <- expr
              return $ Negation x

binaryP = do char '('
             x <- binary
             char ')'
             return x

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
