list = [(a, b, 1000 - a - b) | a <- [1 .. 1000], b <- [1 .. a]]

pythagorean (a, b, c) = a * a + b * b == c * c

(a, b, c) = head $ filter pythagorean list

result = a * b * c
