------Innlevering------
--A
--4.5
sant :: Bool -> Bool -> Bool
sant x y = if (x == True) then 
                if y == True then True else False 
            else False
--eller
--sant x y = if (x==True && y==True) then True else False

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
concat :: [[a]] -> [a]
concat = [] 

--5.9