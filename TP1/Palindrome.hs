import qualified Data.List as L

isPalindrome [] = error "empty string cannot be a palindrome"
isPalindrome xs = xs == reverse xs

removeBlanks = L.filter (\x -> x /= ' ')

isStringPalindrome xs = isPalindrome (removeBlanks xs)