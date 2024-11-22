digit :: (Integral t) => t -> [t]
digit a
  | a >= 10 = mod a 10 : digit (div a 10)
  | otherwise = [a]

build [a, b, c, d] = (d, c, b, a)
build [a, b, c] = (0, c, b, a)
build [a, b] = (0, 0, b, a)
build [a] = (0, 0, 0, a)

list = map (build . digit) [1 .. 1000]

num (0, 0, 0, 0) = 0
num (1, 0, 0, 0) = 8 + num (0, 0, 0, 1)
num (0, 0, 0, 1) = 3
num (0, 0, 0, 2) = 3
num (0, 0, 0, 3) = 5
num (0, 0, 0, 4) = 4
num (0, 0, 0, 5) = 4
num (0, 0, 0, 6) = 3
num (0, 0, 0, 7) = 5
num (0, 0, 0, 8) = 5
num (0, 0, 0, 9) = 4
num (0, 0, 1, 0) = 3
num (0, 0, 1, 1) = 6
num (0, 0, 1, 2) = 6
num (0, 0, 1, 3) = 8
num (0, 0, 1, 4) = 8
num (0, 0, 1, 5) = 7
num (0, 0, 1, 6) = 7
num (0, 0, 1, 7) = 9
num (0, 0, 1, 8) = 8
num (0, 0, 1, 9) = 8
num (0, 0, 2, a) = 6 + num (0, 0, 0, a)
num (0, 0, 3, a) = 6 + num (0, 0, 0, a)
num (0, 0, 4, a) = 5 + num (0, 0, 0, a)
num (0, 0, 5, a) = 5 + num (0, 0, 0, a)
num (0, 0, 6, a) = 5 + num (0, 0, 0, a)
num (0, 0, 7, a) = 7 + num (0, 0, 0, a)
num (0, 0, 8, a) = 6 + num (0, 0, 0, a)
num (0, 0, 9, a) = 6 + num (0, 0, 0, a)
num (0, a, 0, 0) = 7 + num (0, 0, 0, a)
num (0, a, b, c) = 3 + num (0, a, 0, 0) + num (0, 0, b, c)

result = sum $ map num list