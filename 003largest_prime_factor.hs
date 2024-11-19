
largestPrimeFactor :: Integral t => t -> t -> t
largestPrimeFactor n i
  | n == i = n
  | mod n i == 0 = largestPrimeFactor (div n i) i
  | otherwise = largestPrimeFactor n (i + 1)

result = largestPrimeFactor 600851475143 2