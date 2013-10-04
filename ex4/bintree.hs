{-# LANGUAGE InstanceSigs, RankNTypes #-}

import Data.List(foldl')
import Prelude hiding(lookup)
{- Copy paste code from
 - http://functional-programming.it.jyu.fi/TIES343/Chapters/00059_Project_Binary_Tree.html
 -}

data BinaryTree a
 = Tip | 
   Branch a
           (BinaryTree a)
           (BinaryTree a)
           deriving (Show, Eq, Ord)

insert :: Ord a => a -> b -> BinaryTree (a,b) -> BinaryTree (a,b) 
insert key value Tip = Branch (key,value) Tip Tip 
insert key value (Branch (k,v) left right) 
 | key <= k  = Branch (k,v) (insert key value left) right
 | otherwise = Branch (k,v) left (insert key value right)

fromList :: Ord key => [(key,value)] -> BinaryTree (key,value)
--fromList = foldr (\(k,v) tree -> insert k v tree) Tip
fromList = foldl' (\tree (k,v) -> insert k v tree) Tip 


toList :: Ord key => BinaryTree (key,value) -> [(key,value)]
toList Tip = []
toList (Branch kv l r) = kv:(toList l++toList r)
toOrderedList Tip = []
toOrderedList (Branch kv l r) = toOrderedList l ++ (kv:toOrderedList r)


lookup key Tip = Nothing
lookup key1 (Branch (key2,value) left right) 
   | key1 == key2 = Just value
   | key1 < key2 = lookup key1 left
   | otherwise   = lookup key1 right

test_bt_invariant :: (Ord key) => BinaryTree (key,value) -> Bool
test_bt_invariant = test (const True)  
 where
  test _ Tip = True
  test p (Branch (k,v) l r)
      | not (p k) = False
      | otherwise = test (<= k) l && test (> k) r

property_bt_invariant_after_insert :: [(Int,Char)] -> Bool
property_bt_invariant_after_insert = test_bt_invariant . fromList

-- Tree either already has an element with key k
-- or after adding an element with k and removing it will return a tree
-- that has the same values as before. Structure may change.
delete_invariant :: (Ord key, Eq value) => BinaryTree (key,value) -> key -> value -> Bool
delete_invariant bt k v =  (delete k bt) /= bt
                         || (delete k (insert k v bt)) == bt
                        -- || toOrderedList (delete k (insert k v bt)) == toOrderedList bt

property_bt_invariant_after_delete :: [(Int,Char)] -> Int -> Char -> Bool
property_bt_invariant_after_delete b k v = and [ (test_bt_invariant . delete k $ fromList b)
                                               , (delete_invariant (fromList b) k v) ]



largest :: Ord key => BinaryTree (key, value)
            -> Maybe ((key,value), BinaryTree (key, value))
largest Tip 
    = Nothing

largest (Branch kv l Tip) 
    = Just (kv, l)

largest (Branch kv l r@(Branch _ _ _))
    = case largest r of
        Nothing -> Nothing
        Just (large,newRight) -> Just (large, Branch kv l newRight)

-- 

smallest :: Ord key => BinaryTree (key,value)
            -> Maybe ((key,value),BinaryTree (key, value))
smallest Tip 
    = Nothing

smallest (Branch kv Tip r) 
    = Just (kv, r)

smallest (Branch kv l@(Branch _ _ _) r)
    = case smallest l of
        Nothing -> Nothing
        Just (small,newLeft) -> Just (small, Branch kv newLeft r)

delete :: Ord key => key -> BinaryTree (key,value) -> BinaryTree (key,value)
delete key Tip                        = Tip
delete key b@(Branch (k,v) Tip Tip) 
 | key == k  = Tip
 | otherwise = b
delete key b@(Branch (k,v) left Tip)  
 | key == k  = left
 | otherwise = Branch (k,v) (delete key left) Tip
delete key b@(Branch (k,v) Tip right)
 | key == k  = right
 | otherwise = Branch (k,v) Tip (delete key right)
delete key b@(Branch (k,v) left right)
 | key <  k  = Branch (k,v) (delete key left) right
 | key >  k  = Branch (k,v) left (delete key right)
 | key == k  = Branch nr    left right'
  where
    -- Only way that smallest could return Nothing is if 
    -- parameter was Tip. Pattern matching forces right
    -- to be Branch
    Just (nr, right') = smallest right 


size :: BinaryTree (key,value) -> Int
size Tip            = 0
size (Branch _ l r) = 1 + size l + size r

depth :: BinaryTree (key,value) -> Int
depth Tip            = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

instance Functor BinaryTree where
  --fmap :: forall a b c. (b -> c) -> BinaryTree (a,b) -> BinaryTree (a,c) 
  fmap f Tip                = Tip
  fmap f (Branch (k,v) l r) = Branch (k, f v) (fmap f l) (fmap f r)

--map :: Ord a => (b->c) -> BinaryTree (a,b) -> BinaryTree (a,c)
--map = fmap 
