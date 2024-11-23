sumProperDivisor n = sum $ filter (\a -> mod n a == 0) [1 .. n - 1]

result = sum $ filter (\a -> sumProperDivisor a /= a && sumProperDivisor (sumProperDivisor a) == a) [1 .. 10000]