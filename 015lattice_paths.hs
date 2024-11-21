a = [[if x == 0 || y == 0 then 1 else (a !! (x - 1) !! y + a !! x !! (y - 1)) | y <- [0 .. 20]] | x <- [0 .. 20]]

result = a !! 20 !! 20
