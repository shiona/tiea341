
import Control.Applicative 
    -- (We need this later to use the standard implementation
    --  of the applicative class)

newtype Parser a = Parser (String -> [(a,String)])

item :: Parser Char
item = Parser p
 where 
  p (x:xs) = [(x,xs)]
  p ""     = []

runParser :: Parser p -> String -> [(p,String)]
runParser (Parser p) s = p s 

character :: Char -> Parser Char
character x = undefined {- EXERCISE. -}

instance Functor Parser where
   fmap f (Parser p) = Parser $ \s -> map (\(a,b) -> (f a,b)) $ p s

instance Applicative Parser where
   (Parser fs) <*> (Parser xs) = undefined {- EXERCISE 2. 
                                           Hint: See the applicative 
                                                 instance for lists -}
   pure x = undefined {-EXERCISE 1-}

twoItems :: Parser (Char,Char)
twoItems = pure (,) <*> item <*> item

bracket :: Parser p -> Parser p
bracket p = character '(' *> p <* character ')'

-- Things to try:
-- runParser (bracket twoItems) "(12)"

orInstead :: Parser p -> Parser p -> Parser p
orInstead a b = Parser $ \s -> runParser a s ++ runParser b s
