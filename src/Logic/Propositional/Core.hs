module Logic.Propositional.Core where

import Prelude hiding (lookup)

import Control.Monad (replicateM)
import Data.Map (Map (..), fromList, lookup)
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

interpret :: Expr -> Mapping -> Bool
interpret (Variable      v)         vs = fromMaybe False (lookup v vs)
interpret (Negation      expr)      vs = not $ interpret expr vs
interpret (Conjunction   exp1 exp2) vs = interpret exp1 vs && interpret exp2 vs
interpret (Disjunction   exp1 exp2) vs = interpret exp1 vs || interpret exp2 vs
interpret (Conditional   exp1 exp2) vs = not $ interpret exp1 vs || interpret exp2 vs
interpret (Biconditional exp1 exp2) vs = interpret exp1 vs == interpret exp2 vs

assignments :: Expr -> [Mapping]
assignments expr = let vs = variables expr
                       ps = replicateM (length vs) [True, False]
                   in  map (fromList . zip vs) ps

variables :: Expr -> [String]
variables expr = let vars_ (Variable      v)     vs = v : vs
                     vars_ (Negation      e)     vs = vars_ e vs
                     vars_ (Conjunction   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                     vars_ (Disjunction   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                     vars_ (Conditional   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                     vars_ (Biconditional e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
                 in  vars_ expr []

equivalent :: Expr -> Expr -> Bool
equivalent exp1 exp2 = values exp1 == values exp2

isTautology :: Expr -> Bool
isTautology = and . values

isContradiction :: Expr -> Bool
isContradiction = not . or . values

isContingent :: Expr -> Bool
isContingent expr = not (isTautology expr || isContradiction expr)

values :: Expr -> [Bool]
values expr = map (interpret expr) (assignments expr)

showAscii :: Expr -> String
showAscii (Variable      name)      = name
showAscii (Negation      expr)      = '~' : show expr
showAscii (Conjunction   exp1 exp2) = showBC "&"   exp1 exp2
showAscii (Disjunction   exp1 exp2) = showBC "|"   exp1 exp2
showAscii (Conditional   exp1 exp2) = showBC "->"  exp1 exp2
showAscii (Biconditional exp1 exp2) = showBC "<->" exp1 exp2

showBC :: String -> Expr -> Expr -> String
showBC symbol exp1 exp2 =
  '(' : show exp1 ++ " " ++ symbol ++ " " ++ show exp2 ++ ")"
