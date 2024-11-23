import Data.List (sort)
import Data.Text (pack, split, unpack)
import GHC.Base (ord)

names = readFile "0022_names.txt" >>= (return . (filter (/= '"')))

nameList = names >>= (return . nameSpilt)

sortNameList = nameList >>= (return . sort)

valueList = sortNameList >>= (return . map value)

valueListWithIndex = valueList >>= (return . valueWithIndex)

result = valueListWithIndex >>= (return . sum)

nameSpilt a = map unpack $ split (== ',') (pack a)

value [a] = ord a - ord 'A' + 1
value (a : b) = value [a] + value b

valueWithIndex a = [(x + 1) * a !! x | x <- [0 .. length a - 1]]