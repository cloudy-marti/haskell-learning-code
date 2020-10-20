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
reversal _ _ [] = []
reversal i j xs = start ++ rev ++ end
    where
        start = take i xs
        rev = reverse . take (j-i+1) $ drop i xs
        end = drop (j+1) xs

reversalWithSplit :: Int -> Int -> [a] -> [a]
reversalWithSplit _ _ [] = []
reversalWithSplit i j xs = start ++ reverse rev ++ end
    where
        (start, end') = L.splitAt i xs
        (rev, end) = L.splitAt (j-i+1) end'

reversal' :: Int -> Int -> [a] -> [a]
reversal' i l = reversal i (i+l-1)

prefixReversal :: Int -> [a] -> [a]
prefixReversal l = reversal 0 (l-1)

suffixReversal :: Int -> [a] -> [a]
suffixReversal l xs = reversal (len - l) (len - 1) xs
    where
        len = length xs

{-isPrefixReversal :: [a] -> [a] -> Bool
isPrefixReversal xs ys = or tests
    where
        tests = [prefixReversal l xs == ys | l <- [1..length xs]]

isSuffixReversal :: [a] -> [a] -> Bool
isSuffixReversal xs ys = or tests
    where
        tests = [suffixReversal l xs == ys | l <- [1..length xs]]-}

isPrefixReversal' :: Eq a => [a] -> [a] -> Bool
isPrefixReversal' xs ys = reverse xs `L.isPrefixOf` ys