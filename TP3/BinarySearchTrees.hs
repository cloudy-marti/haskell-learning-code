import qualified Data.List as L

insertBST :: (Ord a) => BTree a -> a -> BTree a
insertBST Empty val = Branch Empty val Empty
insertBST (Branch l x r) val
    | val <= x = Branch (insertBST l val) x r
    | otherwhise = Branch l x (insertBST r val)


searchBST :: (Ord a) => BTree a -> a -> Bool
searchBST Empty _ = False
searchBST (Branch l x r) val
    | val == x = True
    | val < x = searchBST l val
    | otherwise = searchBST r val