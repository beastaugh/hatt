{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Logic.Propositional.Parser
    ( parseExpr
    ) where

import Data.Logic.Propositional.Core (Expr (..), Var (..))

import Text.ParserCombinators.Parsec
    ((<|>), char, choice, eof, letter, parse, spaces, string, try)

import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Pos (SourceName)
import Text.ParserCombinators.Parsec.Prim (GenParser)

-- | The 'parseExpr' function accepts the name of a source, and a string to be
-- parsed, and attempts to parse the string as a logical expression of the
-- following forms, where @&#966;@ and @&#968;@ are metalinguistic variables
-- standing for any valid expression.
--
-- * Variables: @\"P\"@, @\"Q\"@, @\"a\"@, @\"b\"@ etc.; basically anything in
--   the character class @[a-zA-Z]@
--
-- * Negation: @\"~&#966;\"@
--
-- * Conjunction: @\"(&#966; & &#968;)\"@
--
-- * Disjunction: @\"(&#966; | &#968;)\"@
--
-- * Conditional: @\"(&#966; -> &#968;)\"@
--
-- * Biconditional: @\"(&#966; \<-> &#968;)\"@
--
-- Top-level expressions where the primary connective is a binary one do not
-- need to be parenthesised. For example, @\"p -> (q & r)\"@ is a valid
-- expression, although @\"(p -> (q & r))\"@ is also fine.
parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = parse statement

statement :: GenParser Char st Expr
statement = do spaces
               x <- try binary <|> expr
               spaces
               eof
               return x

expr :: GenParser Char st Expr
expr = choice [exprP, negation, variable]

variable :: GenParser Char st Expr
variable = do c <- letter
              return $ Variable (Var c)

negation :: GenParser Char st Expr
negation = do char '~'
              spaces
              x <- try expr <|> exprP
              return $ Negation x

exprP :: GenParser Char st Expr
exprP = do char '('
           spaces
           x <- try binary <|> try expr <|> exprP
           spaces
           char ')'
           return x

binary :: GenParser Char st Expr
binary = do x1 <- expr
            spaces
            s  <- choice $ map string ["&", "|", "->", "<->"]
            spaces
            x2 <- try expr <|> exprP
            return $ connective s x1 x2
  where
    connective c = case c of
      "&"   -> Conjunction
      "|"   -> Disjunction
      "->"  -> Conditional
      "<->" -> Biconditional
      _     -> error "Impossible case"
