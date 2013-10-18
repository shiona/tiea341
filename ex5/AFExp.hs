
import Data.Maybe
import Control.Applicative
import qualified Data.Map as M

data Exp = Var String
           | Val Float
           | Add Exp Exp
           | Sub Exp Exp
           | Mul Exp Exp
           | Div Exp Exp
           | Let String Float Exp
           deriving (Eq,Ord,Show)

type Bindings = M.Map String Float

evalEF :: Exp -> Bindings -> Maybe Float
evalEF (Var s)     = M.lookup s     
evalEF (Val i)     = \_ -> Just i
evalEF (Add a b)   = biRel (+) a b
evalEF (Sub a b)   = biRel (-) a b
evalEF (Mul a b)   = biRel (*) a b
evalEF (Div a b)   = biRel (/) a b
evalEF (Let k v e) = evalEF e . (M.insert k v)

biRel f a b = pure (liftA2 f) <*> evalEF a <*> evalEF b

