--6.1
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

--fører til error.
--The function does not terminate, because each application of fac decreases the argument by one, and
--hence the base case is never reached.
fac1 :: Int -> Int
fac1 0 = 1
fac1 n | n > 0 = n * fac1 (n-1)


--6.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)


--6.3
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n-1))
{-
For example:
2 ^ 3
= { applying ^ }
2 * (2 ^ 2)
= { applying ^ }
2 * (2 * (2 ^ 1))
= { applying ^ }
2 * (2 * (2 * (2 ^ 0)))
= { applying ^ }
2 * (2 * (2 * 1))
= { applying * }
8
-}


--6.4
euclid :: Int -> Int -> Int
euclid x y  | x == y = x
            | x > y = euclid (x-y) y
            | y > x = euclid (y-x) x


--6.5
{-
    length[1,2,3]
= {applying length}
    1 + length [2,3]
= {applying length}
    1 + (1 + length [3])
= {applying length}
    1 + (1 + (1 + length []))
= {applying length}
    1 + (1 + (1 + 0)) 
= {applying +}
    3   
-}

{-
    drop 3 [1,2,3,4,5]
= {applying drop}
    drop 2 [2,3,4,5]
= {applying drop}
    drop 1 [3,4,5]
= {applying drop}
    drop 0 [4,5]
= {applying drop}
    [4,5]
-}

{-
    init[1,2,3]
= {applying init}
    1 : init [2,3]
= {applying init}
    1 : 2 : [3]
= {applying init}
    1 : 2 : []
= {list notation}
    [1,2]
-}

--6.6
--a
og :: [Bool] -> Bool
og [] = False
og [True] = True
og (x:xs) = if x then (og xs) else False

--b
concat' :: [[a]] -> [a]
--concat' xss = [e | xs <- xss , e <- xs]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

--c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

--d
nth :: [a] -> Int -> a
--nth [] n = Exception
nth (x:xs) 1 = x
nth (x:xs) n = nth xs (n-1)

--e
elem' :: Eq a => a -> [a] -> Bool
--elem' x [] = False
elem' x (b:bs)  | x == b = True
                | bs == [] = False 
                | otherwise = elem' x bs

--6.7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | y < x = y : merge (x:xs) ys
              
--6.8
halve :: [a] -> ([a],[a])
halve xs = (splitAt((length xs) `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge(msort start) (msort end)
                where (start, end) = halve xs

--6.9
--a calculate the sum of a list of numbers
--b take a given number of elements from the start of a list
--c selsecct the last element of a non-empty list

--Step 1: define the type
--Step 2: enumerate the cases
--Step 3: define the simple cases
--Step 4: define the other cases
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: (Eq t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' n (x:xs)  | n == 0 = []
                | otherwise = x : take' (n-1) xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs