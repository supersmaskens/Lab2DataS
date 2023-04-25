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
empty :: SkewHeap a
empty = Empty


-- | Creates a single node of a SkewHeap without any children.
leaf :: Ord a => a -> SkewHeap a
leaf x = Node x Empty Empty 


-- | Returns the element at the top of a given heap in the form of a 'Maybe'.
-- If the heap is Empty then a 'Nothing' will be returned.
peek :: SkewHeap a -> Maybe a
peek Empty          = Nothing
peek (Node n _ _) = Just n


-- | Returns the element at the top of a given heap. (Currently not exported)
-- Make sure you check for an empty before using this function as it does not have a case for Empty.  
unsafePeek :: SkewHeap a -> a
unsafePeek (Node n _ _) = n


-- | Returns True if a given heap is Empty, False otherwise.
isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False


-- | Removes the element at the top of a given heap.
remove :: Ord a => SkewHeap a -> SkewHeap a
remove (Node _ l r) = merge l r


-- | Adds an element to a heap.
-- Note that the simple 'add' function defaults to a minheap if the given heap is Empty. 
add :: Ord a => a -> SkewHeap a -> SkewHeap a
add x Empty = leaf x
add x sh    = merge sh (leaf x)


-- | Merges two skewheaps. 
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge t1 Empty  = t1
merge Empty t2  = t2
merge t1@(Node n1 l1 r1) t2@(Node n2 l2 r2)
    | n1 <= n2  = assert (invariant (Node n1 (merge t2 r1) l1)) Node n1 (merge t2 r1) l1
    | otherwise = assert (invariant (Node n2 (merge t1 r2) l2)) Node n2 (merge t1 r2) l2


-- More specific functions -----------------------------------

-- | Compares any element with the element at the top of a heap using a given function.
-- If the given heap is Empty then a False will be returned.
compareToPeek :: (a -> b -> Bool) -> a -> SkewHeap b -> Bool
compareToPeek p _ Empty = False
compareToPeek p a sh    = p a (unsafePeek sh)


-- | Applies an IO() function to every element in a heap.
-- Merges remaining heap each call so that the queue is respected. 
applyIO :: Ord a => (a -> IO()) -> SkewHeap a -> IO()
applyIO f Empty          = return()
applyIO f (Node n l r) = f n >> applyIO f (merge l r)


-- | Turns a given heap into a list.
-- Merges remaining heap each call so that the queue is respected. 
toList :: Ord a => SkewHeap a -> [a]
toList Empty          = []
toList (Node n l r) = n : toList (merge l r) 


-- | Creates a list with all the elements of a SkewHeap.
fromList :: Ord a => [a] -> SkewHeap a
fromList [] = Empty
fromList [x] = leaf x
fromList (x:xs) = merge (leaf x) (fromList xs)


-- | Deletes a specific element from a given heap.
-- Uses (==) to find the element.
delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete _ Empty  = Empty
delete x (Node n l r)
    | x == n    = merge l r
    | otherwise = Node n (delete x l) (delete x r) 


-- | Deletes a specific element from a given heap using the given function to find said element.
findAndDelete :: Ord a => (a -> Bool) -> SkewHeap a -> SkewHeap a
findAndDelete p Empty = Empty
findAndDelete p (Node n l r)
    | p n             = merge l r
    | otherwise       = Node n (findAndDelete p l) (findAndDelete p r)




-- Testing Stuff (Not very good :< ) -------------------------

-- Invariant

invariant :: Ord a => SkewHeap a -> Bool
invariant sh = case sh of
    Empty              -> True
    Node n Empty Empty -> True
    Node n l Empty     -> invariant l && n <= unsafePeek l
    Node n Empty r     -> invariant r && n <= unsafePeek r
    Node n l r         -> invariant l &&  invariant r 
                            && n <= unsafePeek l && n <= unsafePeek r
                    

invariantInt :: SkewHeap Int -> Bool
invariantInt = invariant


-- Generator

instance (Arbitrary a, Ord a) => Arbitrary (SkewHeap a) where
    arbitrary = fmap fromList arbitrary














{- instance (Show a) => Show (SkewHeap a) where
    show Empty = "Empty"
    --show (Node n l r _) = "Hey! " ++ show n ++ " " ++ show l ++ " " ++ show r
    show (Node n Empty Empty _) = show n
    show (Node n l@(Node nl _ _ _) Empty _) = "( Node: " ++ show n ++ " Left: " ++ show nl ++ " Right: " ++ "Empty" ++ ")" ++ "\n" ++ show l 
    show (Node n Empty r@(Node nr _ _ _) _) = "( Node: " ++ show n ++ " Left: " ++ "Empty" ++ " Right: " ++ show nr ++ ")" ++ "\n" ++ show r 
    show (Node n l@(Node nl _ _ _) r@(Node nr _ _ _) _) = "( Node: " ++ show n ++ " Left: " ++ show nl ++ " Right: " ++ show nr ++ ")" ++ "\n"  ++ show l ++ "\n"  ++ show r -}

{- maybeIOLeft :: (Ord a, Ord b) => (a -> b -> Bool) -> (a -> b -> IO()) -> a -> QueuePair a b -> IO (QueuePair a b)
maybeIOLeft _ f x (sh1, Empty) = return (addMax x sh1, Empty)
maybeIOLeft p f x (sh1, sh2)
    | p x (peek sh2) = f x (peek sh2) >> return (sh1, removeMin sh2)
    | otherwise = return (addMax x sh1, sh2)


maybeIORight :: (Ord a, Ord b) => (b -> a -> Bool) -> (a -> b -> IO()) -> b -> QueuePair a b -> IO (QueuePair a b)
maybeIORight _ f x (Empty, sh2) = return (Empty, addMin x sh2)
maybeIORight p f x (sh1, sh2) 
    | p x (peek sh1) = f (peek sh1) x >> return (removeMax sh1, sh2)
    | otherwise = return (sh1, addMin x sh2) -}




{- 
comparePeeks :: (a -> b -> Bool) -> SkewHeap a -> SkewHeap b -> Bool
comparePeeks f Empty _ = False
comparePeeks f sh1 sh2 = compareWithPeek f (peek sh1) sh2 -}

{- data SkewValue a = SkewValue a MaxOrMin

instance Eq a => Eq (SkewValue a) where
    (==) :: Eq a => SkewValue a -> SkewValue a -> Bool
    SkewValue x Min == SkewValue y Min = x == y
    SkewValue x Max == SkewValue y Max = x == y
    SkewValue _ Min == SkewValue _ Max = False

instance Ord a => Ord (SkewValue a) where
    (<=) :: Ord a => SkewValue a -> SkewValue a -> Bool
    SkewValue x Min <= SkewValue y Min = x <= y
    SkewValue x Max <= SkewValue y Max = x >= y 
    SkewValue _ Min <= SkewValue _ Max = False
    SkewValue _ Max <= SkewValue _ Min = False


value :: SkewValue a -> a
value (SkewValue v _) = v  

data SkewHeap a = Empty | Node (SkewValue a) (SkewHeap a) (SkewHeap a) -}


{- data MaxOrMin = Max | Min
data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a) MaxOrMin deriving Foldable
-- this is a mess, not done!


compareWithPeek :: (a -> b -> Bool) -> a -> SkewHeap b -> Bool
compareWithPeek f _ Empty = False
compareWithPeek f a sh = f a (peek sh)


comparePeeks :: (a -> b -> Bool) -> SkewHeap a -> SkewHeap b -> Bool
comparePeeks f sh1 = compareWithPeek f (peek sh1)


peeker :: SkewHeap a -> Maybe a
peeker Empty = Nothing
peeker (Node n _ _ _) = Just n


peek :: SkewHeap a -> a
peek (Node n _ _ _) = n


isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False


merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge t1 Empty = t1
merge Empty t2 = t2

merge t1@(Node n1 l1 r1 Min) t2@(Node n2 l2 r2 Min)
    | n1 <= n2  = Node n1 (merge t2 r1) l1 Min
    | otherwise = Node n2 (merge t1 r2) l2 Min

merge t1@(Node n1 l1 r1 Max) t2@(Node n2 l2 r2 Max)
    | n1 >= n2  = Node n1 (merge t2 r1) l1 Max
    | otherwise = Node n2 (merge t1 r2) l2 Max


applyF :: Ord a => (a -> IO()) -> SkewHeap a -> IO()
applyF f Empty = return()
applyF f (Node n l r _) = do
    f n
    applyF f (merge l r)


toList :: Ord a => SkewHeap a -> [a]
toList Empty = []
toList (Node n l r _) = n : toList (merge l r) 


removeMin, removeMax :: Ord a => SkewHeap a -> SkewHeap a
removeMin (Node _ l r _) = merge l r
removeMax                = removeMin 

addMin, addMax :: Ord a => a -> SkewHeap a -> SkewHeap a
addMin x Empty = Node x Empty Empty Min 
addMin x sh    = merge sh (Node x Empty Empty Min)

addMax x Empty = Node x Empty Empty Max  
addMax x sh    = merge sh (Node x Empty Empty Max)


delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete x Empty = Empty
delete x (Node n l r Min)
    | x == n = merge l r
    | otherwise = Node n (delete x l) (delete x r) Min
delete x (Node n l r Max)
    | x == n = merge l r
    | otherwise = Node n (delete x l) (delete x r) Max


find :: Ord a => (a -> Bool) -> SkewHeap a -> a
find p (Node n l r _)
    | p n = n
    | otherwise = find p (merge l r)


findAndDelete :: Ord a => (a -> Bool) -> a -> SkewHeap a -> SkewHeap a
findAndDelete f x sh = delete (find f sh) sh


findAndReplace :: Ord a => (a -> Bool) -> a -> SkewHeap a -> SkewHeap a
findAndReplace f x Empty = Empty
findAndReplace f x sh@(Node _ _ _ Min) = addMin x (delete (find f sh) sh)
findAndReplace f x sh@(Node _ _ _ Max) = addMax x (delete (find f sh) sh)
 -}


-- /////////////////////////////////////////////////////////////////////////////////////////////////


{- 
data SkewHeap a = Empty | Node a (SkewHeap a) (SkewHeap a) deriving Foldable
-- currently needs seperate methods for max heap (functions that end with T are max) and min heap


compareWithPeek :: (a -> b -> Bool) -> a -> SkewHeap b -> Bool
compareWithPeek f _ Empty = False
compareWithPeek f a sh = f a (peek sh)


comparePeeks :: (a -> b -> Bool) -> SkewHeap a -> SkewHeap b -> Bool
comparePeeks f Empty _ = False
comparePeeks f _ Empty = False
comparePeeks f sh1 sh2 = f (peek sh1) (peek sh2)


peeker :: SkewHeap a -> Maybe a
peeker Empty  = Nothing
peeker (Node n _ _) = Just n


peek :: SkewHeap a -> a
peek (Node n _ _) = n


isEmpty :: SkewHeap a -> Bool
isEmpty Empty = True
isEmpty _     = False


merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge t1 Empty = t1
merge Empty t2 = t2
merge t1@(Node n1 l1 r1) t2@(Node n2 l2 r2)
    | n1 <= n2  = Node n1 (merge t2 r1) l1
    | otherwise = Node n2 (merge t1 r2) l2


toList :: Ord a => SkewHeap a -> [a]
toList Empty = []
toList (Node n l r) = n : toList (merge l r) 


applyF :: Ord a => (a -> IO()) -> SkewHeap a -> IO()
applyF f Empty = return()
applyF f (Node n l r) = do
    f n
    applyF f (merge l r)


removeMin :: Ord a => SkewHeap a -> SkewHeap a
removeMin (Node n l r) = merge l r


add :: Ord a => a -> SkewHeap a -> SkewHeap a
add x sh = merge sh (Node x Empty Empty)


delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete x Empty = Empty
delete x (Node n l r)
    | x == n = merge l r
    | otherwise = Node n (delete x l) (delete x r)


find :: Ord a => (a -> Bool) -> SkewHeap a -> a
find p (Node n l r)
    | p n = n
    | otherwise = find p (merge l r)


findAndReplace :: Ord a => (a -> Bool) -> a -> SkewHeap a -> SkewHeap a
findAndReplace f x sh = add x (delete (find f sh) sh)  


findAndDelete :: Ord a => (a -> Bool) -> a -> SkewHeap a -> SkewHeap a
findAndDelete f x sh = delete (find f sh) sh 


mergeT :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeT t1 Empty = t1
mergeT Empty t2 = t2
mergeT t1@(Node n1 l1 r1) t2@(Node n2 l2 r2)
    | n1 >= n2  = Node n1 (mergeT t2 r1) l1
    | otherwise = Node n2 (mergeT t1 r2) l2


toListT :: Ord a => SkewHeap a -> [a]
toListT Empty = []
toListT (Node n l r) = n : toListT (mergeT l r) 


applyFT :: Ord a => (a -> IO()) -> SkewHeap a -> IO()
applyFT f Empty = return()
applyFT f (Node n l r) = do
    f n
    applyFT f (mergeT l r)


removeMinT :: Ord a => SkewHeap a -> SkewHeap a
removeMinT (Node n l r) = mergeT l r


addT :: Ord a => a -> SkewHeap a -> SkewHeap a
addT x sh = mergeT sh (Node x Empty Empty)


deleteT :: Ord a => a -> SkewHeap a -> SkewHeap a
deleteT x Empty = Empty
deleteT x (Node n l r)
    | x == n = mergeT l r
    | otherwise = Node n (deleteT x l) (deleteT x r)


findT :: Ord a => (a -> Bool) -> SkewHeap a -> a
findT p (Node n l r)
    | p n = n
    | otherwise = findT p (mergeT l r)


findAndReplaceT :: Ord a => (a -> Bool) -> a -> SkewHeap a -> SkewHeap a
findAndReplaceT f x sh = addT x (deleteT (findT f sh) sh)  


findAndDeleteT :: Ord a => (a -> Bool) -> a -> SkewHeap a -> SkewHeap a
findAndDeleteT f x sh = deleteT (findT f sh) sh -}




{- a :: Int -> SkewHeap Int
a n = Node n Empty Empty  -}



