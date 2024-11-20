sumOfSquare a = sum [x * x | x <- [1 .. a]]

squareOfSum a = s * s where s = sum [1 .. a]

result = squareOfSum 100 - sumOfSquare 100