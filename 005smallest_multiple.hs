isEvenlyDived :: (Integral t) => t -> t -> Bool
isEvenlyDived n 1 = True
isEvenlyDived n a
  | mod n a == 0 = isEvenlyDived n (a - 1)
  | otherwise = False

result = last (takeWhile (\a -> not $ isEvenlyDived a 20) [1 ..]) + 1