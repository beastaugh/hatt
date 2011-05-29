import Data.Logic.Propositional
import Data.Map (fromList)

main = let expr = Disjunction (Conjunction (Variable "A") (Variable "B")) (Negation (Variable "B"))
           assn = fromList [("A", True), ("B", False)]
       in  putStr $ truthTable expr
