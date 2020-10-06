getIncreasingList 0 = []
getDecreasingList x = x : getIncreasingList (x-1)