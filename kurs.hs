en :: Eq t => [t] -> t -> Bool
en [] _ = False
en (x:xs) n = x == n || en xs n

fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)