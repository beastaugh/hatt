{-# OPTIONS_HADDOCK hide #-}

module Data.Logic.Propositional.Core where

import Prelude hiding (lookup)

import Control.Monad (replicateM)
import Data.List (nub)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)

data Expr = Variable      String
          | Negation      Expr
          | Conjunction   Expr Expr
          | Disjunction   Expr Expr
          | Conditional   Expr Expr
          | Biconditional Expr Expr
          deriving Eq

instance Show Expr where
  show (Variable      name)      = name
  show (Negation      expr)      = '¬' : show expr
  show (Conjunction   exp1 exp2) = showBC "∧" exp1 exp2
  show (Disjunction   exp1 exp2) = showBC "∨" exp1 exp2
  show (Conditional   exp1 exp2) = showBC "→" exp1 exp2
  show (Biconditional exp1 exp2) = showBC "↔" exp1 exp2

type Mapping = Map String Bool

-- | In order to interpret an expression, a mapping from variables to truth
-- values needs to be provided. Truth values are compositional; that's to say,
-- the value of a composite expression (any expression which is not atomic)
-- depends on the truth values of its component parts. For example, the Haskell
-- expression below would evaluate to @False@.
--
-- > interpret
-- >     (Conjunction (Variable "A") (Variable "B"))
-- >     (fromList [("A", True), ("B", False)])
interpret :: Expr -> Mapping -> Bool
interpret (Variable      v)         vs = fromMaybe False (lookup v vs)
interpret (Negation      expr)      vs = not $ interpret expr vs
interpret (Conjunction   exp1 exp2) vs = interpret exp1 vs && interpret exp2 vs
interpret (Disjunction   exp1 exp2) vs = interpret exp1 vs || interpret exp2 vs
interpret (Conditional   exp1 exp2) vs = not $ interpret exp1 vs || interpret exp2 vs
interpret (Biconditional exp1 exp2) vs = interpret exp1 vs == interpret exp2 vs

-- | Generates the possible assignments of variables in an expression.
assignments :: Expr -> [Mapping]
assignments expr = let vs = variables expr
                       ps = replicateM (length vs) [True, False]
                   in  map (fromList . zip vs) ps

-- | Lists the names of variables present in an expression.
variables :: Expr -> [String]
variables expr = let vars_ (Variable      v)     vs = v : vs
                     vars_ (Negation      e)     vs = vars_ e vs
                     vars_ (Conjunction   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                     vars_ (Disjunction   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                     vars_ (Conditional   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                     vars_ (Biconditional e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                 in  nub $ vars_ expr []

-- | Determines whether two expressions are extensionally equivalent (that is,
-- have the same values under all interpretations).
equivalent :: Expr -> Expr -> Bool
equivalent exp1 exp2 = values exp1 == values exp2

-- | Determines whether an expression is tautological.
isTautology :: Expr -> Bool
isTautology = and . values

-- | Determines whether an expression is contradictory.
isContradiction :: Expr -> Bool
isContradiction = not . or . values

-- | Determines whether an expression is contingent (that is, true in at least
-- one interpretation and false in at least one interpretation).
isContingent :: Expr -> Bool
isContingent expr = not (isTautology expr || isContradiction expr)

-- | Lists the values of an expression under all interpretations (that is, all
-- assignments of values to variables).
values :: Expr -> [Bool]
values expr = map (interpret expr) (assignments expr)

-- | Represents expressions using only ASCII characters (the 'show' function
-- pretty-prints expressions using logical symbols only present in extended
-- character sets).
showAscii :: Expr -> String
showAscii (Variable      name)      = name
showAscii (Negation      expr)      = '~' : showAscii expr
showAscii (Conjunction   exp1 exp2) = showBCA "&"   exp1 exp2
showAscii (Disjunction   exp1 exp2) = showBCA "|"   exp1 exp2
showAscii (Conditional   exp1 exp2) = showBCA "->"  exp1 exp2
showAscii (Biconditional exp1 exp2) = showBCA "<->" exp1 exp2

showBinaryConnective :: (Expr -> String) -> String -> Expr -> Expr -> String
showBinaryConnective show_ symbol exp1 exp2 =
  '(' : show_ exp1 ++ " " ++ symbol ++ " " ++ show_ exp2 ++ ")"

showBC :: String -> Expr -> Expr -> String
showBC = showBinaryConnective show

showBCA :: String -> Expr -> Expr -> String
showBCA = showBinaryConnective showAscii
