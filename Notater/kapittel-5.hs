--5.1
--summen av 1^2 til 100^2
summen :: Int
summen = sum [x^2 | x <-[1..100]]

--5.2
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

--5.3
square :: Int -> [(Int, Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], (y/=x)]

--5.4
replicateing :: Int -> a -> [a]
replicateing n x = take n (repeat x)

--5.5
--pyths :: Int -> [(Int,Int,Int)]
--pyths