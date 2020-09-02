--4.1
halve :: [a] -> ([a],[a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)
-- halve xs = splitAt(length xs `div` 2) xs

--4.2
--a
thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

--b
thirdB :: [a] -> a
thirdB xs = xs !! 2

--c
thirdC :: [a] -> a
thirdC (_:_:x:_) = x

--4.3
--safetail :: [a] -> [a]

--a
safetailA :: [a] -> [a]
safetailA xs = if null xs then xs else tail xs

--b
safetailB :: [a] -> [a]
safetailB xs | null xs = xs
             | otherwise = tail xs

--c
safetailC :: [a] -> [a]
--safetailC [] = []
--safetailC xs = tail xs
safetailC [] = []
safetailC (_:xs) = xs


--4.4
(||) :: Bool -> Bool -> Bool
False || False = False 
_ || _ = True

{-
True || True = True
True || False = True
False || True = True
False || False = False
-}

--4.5
sant :: Bool -> Bool -> Bool
sant x y = if (x == True) then 
                if y == True then True else False 
            else False


--4.6

--sant2 :: Bool -> a  -> {a, Bool}
sant2 :: Bool -> Bool -> Bool
sant2 x y = if x == True then y else False 


--4.7
mult :: Int -> Int -> Int -> Int
mult x y z  = x*y*z

mult1 :: Int -> Int -> Int -> Int
mult1 = \x -> (\y -> (\z -> x*y*z))


--4.8
