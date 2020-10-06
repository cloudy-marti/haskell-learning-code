last' [] = error "empty list"
last' (_ : x : []) = x
last' (_ : xs) = last' xs