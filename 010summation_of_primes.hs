impIsPrime a i
  | a < i * i = True
  | mod a i /= 0 = impIsPrime a $ i + 1
  | otherwise = False

isPrime a = impIsPrime a 2

result = sum $ filter isPrime [2 .. 2000000]