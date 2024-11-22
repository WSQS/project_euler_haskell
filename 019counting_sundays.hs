getDays year 2
  | isLeapYear year = 29
  | otherwise = 28
  where
    isLeapYear year
      | mod year 4 == 0 && mod year 100 /= 0 = True
      | mod year 400 == 0 = True
      | otherwise = False
getDays year month
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | otherwise = 30

days = scanl (\a b -> mod (a + b) 7) 1 [mod (getDays year month) 7 | year <- [1900 .. 2000], month <- [1 .. 12]]

result = length $ filter (== 0) $ drop 12 days