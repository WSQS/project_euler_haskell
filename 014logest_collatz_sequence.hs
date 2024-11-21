import Data.Function (on)
import Data.List (maximumBy)
import Debug.Trace (trace)

impLength 1 i = i
impLength n i
  | even n = impLength (div n 2) (i + 1)
  | otherwise = impLength (n * 3 + 1) (i + 1)

length' a = trace (show a ++ ":" ++ show (impLength a 1)) (impLength a 1)

result = fst $ maximumBy (compare `on` snd) [(a, length' a) | a <- [1 .. 1000000]]