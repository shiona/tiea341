
import Data.Maybe
import Control.Applicative
import qualified Data.Map as M

-- Note that this evaluator will only
-- work on lists of Floats

data Exp = Var String
           | Val [Float]
           | Neg Exp
           | Add Exp Exp
           | Sub Exp Exp
           | Mul Exp Exp
           | Div Exp Exp
           | Let String [Float] Exp
           | Sq  Exp
           | PM  Exp Exp
           deriving (Eq,Ord,Show)

type Bindings = M.Map String [Float]

eval = flip evalEF M.empty

evalEF :: Exp -> Bindings -> Maybe [Float]
evalEF (Var s)     = M.lookup s     
evalEF (Val i)     = \_ -> Just i
evalEF (Neg a)     = biRel (-) (Val [0]) a
evalEF (Add a b)   = biRel (+) a b
evalEF (Sub a b)   = biRel (-) a b
evalEF (Mul a b)   = biRel (*) a b
evalEF (Div a b)   = biRel (/) a b
evalEF (Let k v e) = evalEF e . (M.insert k v)
evalEF (Sq  a)     = liftA(liftA(filter (not . isNaN) . liftA sqrt)) (evalEF a)
evalEF (PM  a b)   = liftA2(liftA2(++)) (biRel (+) a b) (biRel (-) a b)

biRel f a b = pure (liftA2 (liftA2 f)) <*> evalEF a <*> evalEF b

-- test cases:

t1 = eval $ quadr 3 (-5) 2
t2 = eval $ quadr 3 (-5) 10

quadr x y z = Let "a" [x] (Let "b" [y] (Let "c" [z] (
              Div (PM (Neg (Var "b")) (Sq (Sub (Mul (Var "b") (Var "b")) (Mul (Mul (Val [4]) (Var "a")) (Var "c"))))) (Mul (Val [2]) (Var "a"))
              )))
