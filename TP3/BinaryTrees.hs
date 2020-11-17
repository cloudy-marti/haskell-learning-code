import qualified Data.List as L
import qualified Data.Maybe as Maybe

data BTree a = Empty | Branch (BTree a) a (BTree a)
    deriving (Show)

exampleBT :: BTree Int
exampleBT = root
    where
        leaf5 = Branch Empty 5 Empty
        node8 = Branch leaf5 8 Empty
        leaf3 = Branch Empty 3 Empty
        leaf15 = Branch Empty 15 Empty
        node4 = Branch leaf3 4 node8
        node20 = Branch leaf15 20 Empty
        root = Branch node4 10 node20

indent :: Int -> String
indent = flip L.replicate '.'

str :: (Show a) => BTree a -> String
str = aux 0
    where
        aux k Empty = indent k ++ "L\n"
        aux k (Branch lt x rt) = indent k ++ show x ++ "\n" ++ aux (k+1) lt ++ aux (k+1) rt

emptyBT :: BTree a
emptyBT = Empty

size :: Num b => BTree a -> b
size Empty = 0
size (Branch Empty _ Empty) = 1
size (Branch l _ r) = 1 + size l + size r

maxBT :: (Ord a, Bounded a) => BTree a -> a -- Integer is not bounded -> use int
maxBT Empty = minBound
maxBT (Branch l x r) = maximum [lx, x, rx]
    where
        lx = maxBT l 
        rx = maxBT r

minBT :: (Ord a, Bounded a) => BTree a -> a
minBT Empty = maxBound
minBT (Branch l x r) = minimum [lx, x, rx]
    where
        lx = minBT l 
        rx = minBT r

minOrMax :: ([a] -> a) -> BTree a -> a -> a
minOrMax _ Empty bound = bound
minOrMax lambda (Branch l x r) bound = lambda [lx, x, rx]
    where
        lx = minOrMax lambda l bound
        rx = minOrMax lambda r bound

maxBT' :: (Ord a, Bounded a) => BTree a -> a
maxBT' t = minOrMax maximum t minBound

minBT' :: (Ord a, Bounded a) => BTree a -> a
minBT' t = minOrMax minimum t maxBound

maxBT'' :: (Ord a) => BTree a -> Maybe a
maxBT'' Empty = Nothing
maxBT'' (Branch l x r) = maximum [maxBT'' l, Just x, maxBT'' r]

minBT'' :: (Ord a) => BTree a -> Maybe a
minBT'' Empty = Nothing
minBT'' (Branch Empty x Empty) = Just x
minBT'' (Branch l x Empty) = minimum [minBT'' l, Just x]
minBT'' (Branch Empty x r) = minimum [minBT'' r, Just x]
minBT'' (Branch l x r) = minimum [minBT'' l, Just x, minBT'' r]

height :: (Ord b, Num b) => BTree a -> b
height Empty = 0
height (Branch l _ r) = 1 + maximum [height l, height r]

searchBT :: Eq a => BTree a -> a -> Bool
searchBT Empty _ = False
searchBT (Branch l x r) val
    | val == x = True
    | otherwise = (searchBT l x) || (searchBT r x)

toList :: BTree a -> [a] -- same as prefix visit
toList Empty = []
toList (Branch l x r) = x : toList l ++ toList r

preVisit :: BTree a -> [a]
preVisit Empty = []
preVisit (Branch l x r) = x : preVisit l ++ preVisit r

inVisit :: BTree a -> [a]
inVisit Empty = []
inVisit (Branch l x r) = inVisit l ++ x : inVisit r

postVisit :: BTree a -> [a]
postVisit Empty = []
postVisit (Branch l x r) = postVisit l ++ postVisit r ++ [x]

toList' :: (BTree a -> [a]) -> BTree a -> [a]
toList' lambda tree = lambda tree

filterBT :: (a -> Bool) -> BTree a -> [a]
filterBT lambda tree = filter lambda $ preVisit tree

mapBT :: (a -> b) -> BTree a -> BTree b
mapBT _ Empty = Empty
mapBT lambda (Branch l x r) = (Branch newL newX newR)
    where
        newL = mapBT lambda l
        newX = lambda x
        newR = mapBT lambda r

insertBST :: (Ord a) => BTree a -> a -> BTree a
insertBST Empty val = Branch Empty val Empty
insertBST (Branch l x r) val
    | val <= x = Branch (insertBST l val) x r
    | otherwise = Branch l x (insertBST r val)


searchBST :: (Ord a) => BTree a -> a -> Bool
searchBST Empty _ = False
searchBST (Branch l x r) val
    | val == x = True
    | val < x = searchBST l val
    | otherwise = searchBST r val

deleteLargestBST :: BTree a -> Maybe (a, BTree a)
deleteLargestBST Empty = Nothing
deleteLargestBST (Branch l x Empty) = Just (x, l)
deleteLargestBST (Branch l x r) = Just (largest, Branch l x rlargest)
    where
        Just (largest, rlargest) = deleteLargestBST r

deleteBST :: (Ord a) => BTree a -> a -> BTree a
deleteBST Empty _ = Empty
deleteBST bst@(Branch Empty x r) y
    | x == y = r
    | y > x = Branch Empty x (deleteBST r y)
    | otherwise = bst
deleteBST (Branch l x r) y
    | x == y = Branch l' x' r
    | y < x = Branch (deleteBST l y) x r
    | otherwise = Branch l x (deleteBST r y)
        where
            Just (x', l') = deleteLargestBST l

mkBST :: [a] -> Maybe BTree a
mkBST [] = Nothing
mkBST x : xs = root
    where
        root = insertBST (Branch Empty x Empty)
        