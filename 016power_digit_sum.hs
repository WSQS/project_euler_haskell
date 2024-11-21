digit :: (Integral t) => t -> [t]
digit a
  | a >= 10 = mod a 10 : digit (div a 10)
  | otherwise = [a]

result = sum $ digit (2 ^ 1000)