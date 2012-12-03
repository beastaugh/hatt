{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Logic.Propositional.Parser
    ( parseExpr
    ) where

import Data.Logic.Propositional.Core (Expr (..), Var (..))

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr

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
parseExpr = runP statement ()

statement :: ParsecT String u Identity Expr
statement = do spaces
               x <- expr
               spaces
               eof
               return x

expr :: ParsecT String u Identity Expr
expr = buildExpressionParser operators term
    <?> "compound expression"

term :: ParsecT String u Identity Expr
term =  parens expr
    <|> variable
    <?> "expression"

variable :: ParsecT String u Identity Expr
variable = do c <- letter
              spaces
              return $ Variable (Var c)
        <?> "variable"

parens :: ParsecT String u Identity Expr -> ParsecT String u Identity Expr
parens p = do char '('
              spaces
              x <- p
              char ')'
              spaces
              return x
        <?> "parens"

operators :: OperatorTable String u Identity Expr
operators = [ [Prefix (string "~" >> return Negation)]
            , [binary "&" Conjunction]
            , [binary "|" Disjunction]
            , [binary "->" Conditional]
            , [binary "<->" Biconditional]
            ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator String u Identity Expr
binary n c = Infix (string n >> spaces >> return c) AssocRight
