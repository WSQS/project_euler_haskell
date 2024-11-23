fibonacci :: (Num a) => (a, a) -> (a, a)
fibonacci (a, b) = (b, a + b)

digit a
  | a >= 10 = mod a 10 : digit (div a 10)
  | otherwise = [a]

digitLength = length . digit

fibonacciList = zip [1 ..] $ map fst $ iterate fibonacci (1, 1)

digitList = map (\(a, b) -> (a, digitLength b)) fibonacciList

filterList = takeWhile (\(a, b) -> b < 1000) digitList

result = fst (last filterList) + 1