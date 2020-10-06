init' [] = error "empty list"
init' [x] = []
init' (x : xs) = x : init' xs