---- Innlevering ----
--1
--al :: [Bool] -> Bool
--a
alRec :: [Bool] -> Bool
alRec [] = True
alRec (x:xs)    | x = alRec xs
                | otherwise = False

--timen
al1 :: [Bool] -> Bool
al1 [] = True
al1 (False:_) = False
al1 (_:ls) = al1 ls 


--b lik som i :timen
alFunc :: [Bool] -> Bool
alFunc xs = all (== True) xs

--c
alFoldl :: [Bool] -> Bool
alFoldl xs = foldl (==) (True) xs
--timen
--alFoldl' xs = foldl (&&) True xs
 
--d
alFoldr :: [Bool] -> Bool
alFoldr xs = foldr (==) (True) xs
--timen
--alFoldr' xs = foldr (&&) True xs

--2
ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala _ b [] = b --tar ikke denne med i timen, er unødvendig
ala p b (xs) = foldl p b xs


--timen
ala1 :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala1 _ b [] = b
ala1 func _ ls = foldl func (head ls) (tail ls)

--3
trekant :: Int -> IO()
trekant x = putStr (tegne x 1)

tegne :: (Eq t, Num t) => t -> Int -> [Char]
tegne 0 _ = ""
tegne x y = concat (replicate y "* ") ++ "\n" ++ tegne (x-1) (y+1)

--timen
trekant2 :: Int -> IO()
trekant2 x = do
        putStr $ trekanhjelper x

trekanhjelper :: Int -> String
trekanhjelper n 
                | n < 1 = []
                | otherwise = trekanhjelper (n-1) ++ (replicate n '*') ++ "\n"

--4
trekant' :: Int -> IO()
trekant' x = putStr (tegne' x 1 x)

tegne' :: (Eq t, Num t) => t -> Int -> Int -> [Char]
tegne' 0 _ _ = ""
tegne' x y z =  concat (replicate z " ") ++ concat (replicate y "* ") ++ "\n" ++ tegne' (x-1) (y+1) (z-1)

--timen
juletre :: Int -> IO()
juletre n = do --viktig pga putstr og IO()
        putStr $ juletrehjelper n 0

juletrehjelper :: Int -> Int -> String
juletrehjelper n m 
            | n < 1 = []
            |otherwise = (juletrehjelper (n-1) (m+1)) ++ (replicate m ' ') ++ concat (replicate n "* ") ++ "\n"