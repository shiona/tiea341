
module LookupBot where
import Data.Maybe
import SimpleIO -- See the "Programs and IO" section 

findElem :: (Eq a) => a -> [(a,b)] -> Maybe b
findElem a [] = Nothing
findElem a ((x,y):xs)
 | x==a      = Just y
 | otherwise = findElem a xs 

dictionary :: [(String,[String])]
dictionary = [("aid",["help","render assistance"])
            ,("ask",["inquire","question"])
            
            ]

main :: IO ()
main = questionAnswer 
        "This is a dictionary service. Type enter to quit."
        (\word -> case findElem word dictionary of
                   Nothing -> Just "No such word found"
                   Just words -> Just $ unlines words)


