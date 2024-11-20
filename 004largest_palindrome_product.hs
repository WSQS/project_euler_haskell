digit :: (Integral t) => t -> [t]
digit a
  | a >= 10 = mod a 10 : digit (div a 10)
  | otherwise = [a]

palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome a
  | head a /= last a = False
  | otherwise = palindrome $ tail $ init a

isPalindrome :: (Integral a) => a -> Bool
isPalindrome a = palindrome $ digit a

palindromeList = filter isPalindrome [a * b | a <- [100 .. 999], b <- [100 .. 999]]

result = maximum palindromeList
