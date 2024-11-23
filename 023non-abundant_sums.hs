import Data.List (nub)
import Debug.Trace (trace)

sumProperDivisor n = sum $ filter (\a -> mod n a == 0) [1 .. div n 2]

isAbundantNumber a = a < sumProperDivisor a

abundantNumberList = filter isAbundantNumber [1 .. 28123]

nonAbundantSumList = filter (\a -> trace (show a) not $ isAbundantSum a 0) [1 .. 28123]

nonAbundantSum = sum nonAbundantSumList

isAbundantSum a m
  | m >= length abundantNumberList = False
  | abundantNumberList !! m >= a = False
  | elem (a - abundantNumberList !! m) abundantNumberList = True
  | otherwise = isAbundantSum a (m + 1)