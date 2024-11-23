import Data.List (permutations, sort)

list = sort $ permutations [0 .. 9]

result = list !! 999999