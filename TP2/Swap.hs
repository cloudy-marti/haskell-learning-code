import qualified Data.List as L

-- Question 1
-- 1.
mirror :: Eq a => [a] -> [a] -> Bool
mirror xs ys = xs == reverse ys

-- 2.
permute :: Eq a => [a] -> [a] -> Bool
permute [] [] = True
permute [] _ = False
permute (x : xs) ys
    | x `elem` ys = permute xs (L.delete x ys)
    | otherwise = False

-- 3
permute' :: Ord a => [a] -> [a] -> Bool
permute' xs ys = L.sort xs == L.sort ys

-- Question 2
-- 1.
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = []
pairs (x1 : x2 : xs) = (x1, x2) : (pairs (x2 : xs))

-- 2.
evenElts :: [a] -> [a]
evenElts [] = []
evenElts [x] = [x]
evenElts (x1 : x2 : xs) = x1 : evenElts xs

-- 3.
subLength :: [[a]] -> [([a], Int)]
subLength = map (\ x -> (x, length x))

-- 4.
appOnPairs :: (a -> c) -> (b -> d) -> [(a, b)] -> [(c, d)]
appOnPairs f g = map (\ (x, y) -> (f x, g y))

-- 5
factors :: (Eq a) => [a] -> [[a]]
factors [] = []
factors w@(_ : xs) = L.nub $ L.inits w ++ factors xs

-- 6
subseqs :: (Eq a) => [a] -> [[a]]
subseqs [] = [[]]
subseqs (x : xs) = ss ++ map (x:) ss
    where
        ss = subseqs xs

-- Question 3 : Renversements ou Permutations particulières
-- 1.
reversal :: Int -> Int -> [a] -> [a]
reversal x y [] = []
reversal x y xs = 