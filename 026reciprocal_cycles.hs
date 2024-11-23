import Data.List (maximumBy)
import Data.Ord (comparing)
import Debug.Trace (trace)

digit a
  | a >= 10 = mod a 10 : digit (div a 10)
  | otherwise = [a]

digitLength = length . digit

recurringCycleLength' n a
  | mod a n == 1 = digitLength a - 1
  | otherwise = recurringCycleLength' n (a * 10)

recurringCycleLength a
  | mod a 2 == 0 || mod a 5 == 0 = 0
  | otherwise = recurringCycleLength' a 10

result = fst $ maximumBy (comparing snd) [(x, recurringCycleLength x) | x <- [2 .. 999]]