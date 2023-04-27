module PrioQueueLab2 
(SkewHeap,
 empty, leaf,
 compareToPeek, peek, isEmpty,
 remove, add,
 delete, findAndDelete, applyIO
 ) where

import Control.Exception (assert)
import Test.QuickCheck 
import Data.Maybe (fromJust)






-- | Skewheap type that can be either a max heap or a min heap.
data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a) deriving Show
-- this is a mess, not done



-- Basic Functions -------------------------------------------

-- | Creates an empty SkewHeap

-- Complexity : O(1)
empty :: SkewHeap a
empty = Empty


-- | Creates a single node of a SkewHeap without any children.

-- Complexity : O(1)
leaf :: Ord a => a -> SkewHeap a
leaf x = Node x Empty Empty 


-- | Returns the element at the top of a given heap in the form of a 'Maybe'.
-- If the heap is Empty then a 'Nothing' will be returned.

-- Complexity : O(1) (This is assuming the Maybe doesn't add any)
peek :: SkewHeap a -> Maybe a
peek Empty          = Nothing
peek (Node n _ _) = Just n


-- | Returns the element at the top of a given heap. (Currently not exported)
-- Make sure you check for an empty before using this function as it does not have a case for Empty.  

-- Complexity : O(1)
unsafePeek :: SkewHeap a -> a
unsafePeek (Node n _ _) = n


-- | Returns True if a given heap is Empty, False otherwise.

-- Complexity : O(1)
isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False


-- | Removes the element at the top of a given heap.

-- Complexity: O(log(n - 1)) = O(log n)    | Kanske räcker med O(log n)
remove :: Ord a => SkewHeap a -> SkewHeap a
remove (Node _ l r) = merge l r


-- | Adds an element to a heap.

-- Complexity: O(log n) 
add :: Ord a => a -> SkewHeap a -> SkewHeap a
add x Empty = leaf x
add x sh    = merge sh (leaf x)


-- | Merges two skewheaps.

-- Complexity: O(log n) (Amortized)  | Osäker om denna, behöver vi skriva mer?
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge t1 Empty  = t1
merge Empty t2  = t2
merge t1@(Node n1 l1 r1) t2@(Node n2 l2 r2)
    | n1 <= n2  = Node n1 (merge t2 r1) l1
    | otherwise = Node n2 (merge t1 r2) l2


-- More specific functions -----------------------------------

-- | Compares any element with the element at the top of a heap using a given function.
-- If the given heap is Empty then a False will be returned.

-- Compexity: O(1) + complexity of p        (In our code this is only used for comparisons.)
compareToPeek :: (a -> b -> Bool) -> a -> SkewHeap b -> Bool
compareToPeek p _ Empty = False
compareToPeek p a sh    = p a (unsafePeek sh)


-- | Applies an IO() function to every element in a heap, 
-- the second IO() function is only applied to the last element.
-- Merges remaining heap each call so that the queue is respected.

{-
Complexity: We merge the heap n times and apply a function each time.
The difference in complexity between the two functions can be seen as a constant.
This gives us a complexity of O(n * log n) * complexity of f
-}
applyIO :: Ord a => (a -> IO()) -> (a -> IO()) -> SkewHeap a -> IO()
applyIO f p Empty                = return()
applyIO f p (Node n Empty Empty) = p n
applyIO f p (Node n l r)         = f n >> applyIO f p (merge l r)


-- | Turns a given heap into a list.
-- Merges remaining heap each call so that the queue is respected.

-- Complexity: Calls merge n times so complexity is O(n * log n)
toList :: Ord a => SkewHeap a -> [a]
toList Empty          = []
toList (Node n l r) = n : toList (merge l r) 


-- | Creates a list with all the elements of a SkewHeap.

{-
Complexity: Calls merge once for each element except for the last
so the complexity is O((n-1) * log (n-1)) = O(n * log n)
-}
fromList :: Ord a => [a] -> SkewHeap a
fromList [] = Empty
fromList [x] = leaf x
fromList (x:xs) = merge (leaf x) (fromList xs)


-- | Deletes a specific element from a given heap.
-- Uses (==) to find the element.

{-
Complexity: O(n) while searching for the element and O(log n) (from merge) after its found. 
Worst case scenario is if the element doesn't exist in which case complexity is O(n)
-}
delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete _ Empty  = Empty
delete x (Node n l r)
    | x == n    = merge l r
    | otherwise = Node n (delete x l) (delete x r) 


-- | Deletes a specific element from a given heap using the given function to find said element.

{- 
Complexity: Similar to delete but here we have to account for the complexity of the given function.
O(n) * complexity of p       (In our code this is only used for comparisons.) 
-}
findAndDelete :: Ord a => (a -> Bool) -> SkewHeap a -> SkewHeap a
findAndDelete p Empty = Empty
findAndDelete p (Node n l r)
    | p n             = merge l r
    | otherwise       = Node n (findAndDelete p l) (findAndDelete p r)




-- Testing Stuff (Not very good :< ) -------------------------

-- Invariant (Used for assert)

-- Complexity: Makes a comparison on every element in the heap so the complexity is O(n)
invariant :: Ord a => SkewHeap a -> Bool
invariant sh = case sh of
    Empty              -> True
    Node n Empty Empty -> True
    Node n l Empty     -> invariant l && n <= unsafePeek l
    Node n Empty r     -> invariant r && n <= unsafePeek r
    Node n l r         -> invariant l &&  invariant r 
                            && n <= unsafePeek l && n <= unsafePeek r
                    
-- Use this with QuickCheck
invariantInt :: SkewHeap Int -> Bool
invariantInt = invariant


-- Generator

instance (Arbitrary a, Ord a) => Arbitrary (SkewHeap a) where
    arbitrary = fmap fromList arbitrary








