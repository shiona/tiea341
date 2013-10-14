
module LookupBot where
import Data.Maybe
import Data.List.Split (splitOn)
--import SimpleIO -- See the "Programs and IO" section 

questionAnswer greeting op = putStrLn greeting >> loop
  where 
   loop = do
    x <- getLine
    if null x 
        then return ()
        else case op x of
          Nothing -> return ()
          Just r  -> putStrLn r>>loop    

findWords :: (Eq a) => a -> [(a,b)] -> [b]
findWords x = map snd . filter ((==x).fst) 

dictionary :: [(String, [(String,[String])] ) ]
dictionary = [("misc", 
                  [("aid",["help","render assistance"])
                  ,("ask",["inquire","question"])
                  ])
             ,("programming", 
                  [("c",["foobar [Sic]"])
                  ])
             ,("letters", 
                  [("c",["after b", "before d"])
                  ])
            ]
buildQuery cat word = filter ((== word).fst) . concatMap snd . categoryFilter cat

parseInput inp = case length ls of
                  2         -> Just (x,y)
                  otherwise -> Nothing
  where 
    (x:y:_) = ls
    ls = splitOn "/" inp

-- if this wasn't an exercise in Maybe, then the following would make more sense:
-- categoryFilter s
--  | x == "?"  = id
--  | otherwise = filter ((==x).fst)

categoryFilter :: String -> [(String,a)] -> [(String,a)]
categoryFilter = maybe id (\x -> filter ((==x).fst)) . (\s -> case s of { "?" -> Nothing; _ -> Just s })


main :: IO ()
main = questionAnswer 
        "This is a dictionary service. Type enter to quit."
        (\inp -> case parseInput inp of
            Nothing     -> Just "Query must be of form 'Category/Word', where '?' is a wildcard for categories"
            Just (c, w) -> Just $ unlines . concatMap snd $ (buildQuery c w) dictionary
        )
