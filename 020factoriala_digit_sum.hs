digit :: (Integral t) => t -> [t]
digit a
  | a >= 10 = mod a 10 : digit (div a 10)
  | otherwise = [a]

factorial 1 = 1
factorial n = n * factorial (n - 1)

result = sum $ digit $ factorial 100