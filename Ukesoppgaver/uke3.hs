------Innlevering------
--A
--4.5
sant :: Bool -> Bool -> Bool
sant x y = if (x == True) then 
                if y == True then True else False 
            else False
{-eller
sant x y = if (x==True && y==True) then True else False-}

--4.7
mult :: Int -> Int -> Int -> Int
mult x y z  = x*y*z

mult1 :: Int -> Int -> Int -> Int
mult1 = \x -> (\y -> (\z -> x*y*z))


--B
--5.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x)]

--5.7
--[(x,y) | x <- [1,2], y <- [3,4]]
fem :: [(Int,Int)]
fem = concat [ [(x,y) | y <- [3,4]] | x <- [1,2] ]

--5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [(x*y) | (x,y) <- zip xs ys]


--C
rem1 :: Eq a => [a] -> a -> [a]
--rem1 xs x = [y | y <- xs, y /= x ]
--Denne fjerner alle som er lik x
rem1 [] n = []
rem1 (x:xs) n | n == x = xs
              | otherwise = x : rem1 xs n


--D
--diff :: Eq a => [a] -> [a] -> [a]
--diff xs ys = [zs | zs <- xs, zs /= ys ]

--E
--4.8
luhnDouble :: Int -> Int
luhnDouble x = if(x*2 > 9)  then (x*2)-9 else x*2

luhn ::Int -> Int -> Int -> Int -> Bool
luhn a b c d = if (d + (luhnDouble c) + b + (luhnDouble a)) `mod`  10 == 0 then True else False
