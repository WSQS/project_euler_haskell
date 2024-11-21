import Data.List (group)
import Debug.Trace (trace)

impPrimeFactor n i primeList
  | n <= i = i : primeList
  | mod n i == 0 = impPrimeFactor (div n i) i (i : primeList)
  | otherwise = impPrimeFactor n (i + 1) primeList

primeFactor i = impPrimeFactor i 2 []

numDivisors x = trace (show x ++ ":" ++ show num) num
  where
    num = product [k + 1 | k <- map length $ group $ primeFactor x]

triangleList = [div (a * (a + 1)) 2 | a <- [1 ..]]

result = head $ filter (\a -> numDivisors a > 500) triangleList
