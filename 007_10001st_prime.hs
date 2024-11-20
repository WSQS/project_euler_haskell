impIsPrime a i
  | a == i = True
  | mod a i /= 0 = impIsPrime a $ i + 1
  | otherwise = False

isPrime a = impIsPrime a 2

result = filter isPrime [2 ..] !! 10000