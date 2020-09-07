------Innlevering------
--A
--4.5 riktig men komplisert se sant2
sant :: Bool -> Bool -> Bool
sant x y = if (x == True) then 
                if y == True then True else False 
            else False

sant2 :: Bool -> Bool -> Bool
sant2 x y = if x 
            then (if y then True else False)
            else False

--4.7 riktig
mult :: Int -> Int -> Int -> Int
mult x y z  = x*y*z

mult1 :: Int -> Int -> Int -> Int
mult1 = \x -> (\y -> (\z -> x*y*z))


--B
--5.6 riktig, men isteden for n-1 i factors så bruker du sum(init(factors x))
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x)]

--5.7 riktig
--[(x,y) | x <- [1,2], y <- [3,4]]
fem :: [(Int,Int)]
fem = concat [ [(x,y) | y <- [3,4]] | x <- [1,2] ]

--5.9 riktig
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [(x*y) | (x,y) <- zip xs ys]


--C fire metoder denne er Guarded expression 
--vil si dette er riktig bare ikke den beste løsningen
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] n = []
rem1 (x:xs) n | n == x = xs
              | otherwise = x : rem1 xs n
--kan legge basecase inn i hele.
--En annen løsning v
{-
rem1 xs n   | xs == [] = []
                | head xs == n = tail xs
                | otherwise = (head xs) : (rem1 (tail xs) n)
-}


--conditional expression
rem11 :: Eq a => [a] -> a -> [a]
rem11 [] _ = []
rem11 (x:xs) el = if (x == el) 
                    then xs
                    else x : (rem11 xs el)

--List comprehension
rem12 :: Eq a => [a] -> a -> [a]
rem12 xs el = [x | (x,n) <- zip xs [0..], x /= el || n > length (takeWhile (/=el) xs)]

--Using where-keyword
rem13 xs el = beforeEl ++ (if afterEl == [] then [] else (afterEl))
                where
                    beforeEl = takeWhile (/=el) xs
                    afterEl = dropWhile (/=el) xs


--D riktig men undvenig komplisert
diff :: Eq a => [a] -> [a] -> [a]
diff zs [] = zs
diff (z:zs) (y:ys) | [z' | z' <- (z:zs), z' == y] == [] = diff (z:zs) ys
                   | otherwise = diff (rem1 (z:zs) y) ys

--beste løsning er pattern matching
diff2 :: Eq a => [a] -> [a] -> [a]
diff2 xs [] = xs
diff2 xs (e:els) = diff2 (rem1 xs e) els

--to andre løsninger ved bruk av funksjonene foldl og foldr:
diff3 :: Eq a => [a] -> [a] -> [a]
diff3 xs els = foldl rem1 xs els
--foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

diff4 :: Eq a => [a] -> [a] -> [a]
diff4 xs els = foldr (\x y -> rem1 y x) xs els
--foldl :: Foldable t => (a -> b -> b) -> b -> t a -> b


--E
--4.8
luhnDouble :: Int -> Int
luhnDouble x = if(x*2 > 9)  then (x*2)-9 else x*2

luhn ::Int -> Int -> Int -> Int -> Bool
luhn a b c d = if (d + (luhnDouble c) + b + (luhnDouble a)) `mod` 10 == 0 then True else False


--flere løsninger:
luhnDouble2 :: Int -> Int
luhnDouble2 x   | x*2 > 9 = (x*2)-9 
                | otherwise = x*2

luhnDouble3 :: Int -> Int
luhnDouble3 x = let d = x*2 in
                    if(d > 9) 
                        then d-9 
                        else d

luhn2 ::Int -> Int -> Int -> Int -> Bool
luhn2 x y z w = (luhnDouble x) + y + (luhnDouble z) + w `mod` 10 == 0