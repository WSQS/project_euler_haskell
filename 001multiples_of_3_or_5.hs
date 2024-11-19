list = filter (\a -> a `mod` 3 == 0 || a `mod` 5 == 0) [1 .. 1000]

result = sum list