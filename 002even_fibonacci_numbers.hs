fibonacci :: (Num a) => (a, a) -> (a, a)
fibonacci (a, b) = (b, a + b)

dual_fibonacci :: (Num a) => (a, a) -> (a, a)
dual_fibonacci (a, b) = fibonacci $ fibonacci (a, b)

even_fibonacci_list = map snd $ iterate fibonacci (1, 2)

head_list = takeWhile (< 4000000) even_fibonacci_list

even_list = [a| a<-head_list,mod a 2 ==0]

result = sum even_list