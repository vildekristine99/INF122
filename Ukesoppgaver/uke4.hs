---------Innlevering---------

--A
fjern :: String -> Char -> String
fjern xs n = [x | x <- xs , x /= n]

fjern2 :: String -> Char -> String
fjern2 [] _ = []
fjern2 (x:xs) n | x /= n = x : fjern2 xs n
                | otherwise = fjern2 xs n


--B
tegnpos :: Char -> String -> [Int]
tegnpos n xs = [y | y <- [0..length xs-1], xs !! y == n]

--C
intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList(x `div` 10) ++ [(x `mod` 10)]


--D
--uten bruk av unwords og words
--a
settSammen :: [String] -> String
settSammen [x] = x
settSammen (x:xs) = x ++ " " ++ settSammen xs

--b
delStrengen :: String -> [String]
delStrengen [] = []
delStrengen (x:xs)  
    | x == ' ' = delStrengen (xs)
    | otherwise = vent (x:xs) : delStrengen(drop (length (vent (x:xs))) xs)

vent :: String -> String
vent [] = []
vent (x:xs) | x == ' ' = []
            | otherwise = [x] ++ vent xs



--c
gdelStrengen :: String -> String -> [String]
gdelStrengen [] _ = []
gdelStrengen (x:xs) (ns)  
    | elem x ns = gdelStrengen xs ns
    | otherwise = ventTil (x:xs) (ns) : gdelStrengen(drop (length (ventTil (x:xs) (ns))) xs) (ns)

ventTil :: String -> String -> String
ventTil [] n = []
ventTil (x:xs) (ns) 
    | elem x ns = []
    | otherwise = [x] ++ ventTil xs ns
