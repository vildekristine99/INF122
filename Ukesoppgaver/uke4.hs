---------Innlevering---------

--A
--ritkig
fjern :: String -> Char -> String
fjern xs n = [x | x <- xs , x /= n]

--fungerer
fjern2 :: String -> Char -> String
fjern2 [] _ = []
fjern2 (x:xs) n | x /= n = x : fjern2 xs n
                | otherwise = fjern2 xs n

--løsning fra timen
fjern1 :: String -> Char -> String
fjern1 [] _ = []
fjern1 (s:ss) c = if s == c then fjern1 ss c else s : (fjern1 ss c)


--B
tegnpos :: Char -> String -> [Int]
tegnpos n xs = [y | y <- [0..length xs-1], xs !! y == n]

--løsning fra timen
tegnposL :: Char -> String -> [Int]
tegnposL c ss = [n | (s,n) <- zip ss [0..], s == c]

--løsning på rekursiv 1 
tegnposR :: Char -> String -> [Int]
tegnposR c ss = hjelper_tp1 c ss 0

hjelper_tp1 :: Char -> String -> Int -> [Int]
hjelper_tp1 c [] _ = []
hjelper_tp1 c (s:ss) n = if s==c 
                            then n : (hjelper_tp1 c ss (n+1)) 
                            else (hjelper_tp1 c ss (n+1))

--rekursiv 2
tegnposR2 :: Char -> String -> [Int]
tegnposR2 c ss = hjelper_tp2 c (zip ss [0..])

hjelper_tp2 :: Char -> [(Char, Int)] -> [Int]
hjelper_tp2 _ [] = []
hjelper_tp2 c ((s,n):ts)    | s == c = n : (hjelper_tp2 c ts)
                            | otherwise = hjelper_tp2 c ts 

--C Fungerer
intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList(x `div` 10) ++ [(x `mod` 10)]

--løsning fra timen
intToList2 :: Int -> [Int]
intToList2 n 
                | d == 0 = [r]
                | otherwise = intToList2 d ++ [r]
                where
                    (d,r) = n `divMod` 10
                    --r = n `mod` 10
                    --d = n `div` 10

--den lette måten med read og show
intToList3 :: Int -> [Int]
intToList3 n = [read [c] |c <- show n]

--D
--uten bruk av unwords og words

--a riktig men mangler
--settSammen [] = []
settSammen :: [String] -> String
settSammen [x] = x
settSammen (x:xs) = x ++ " " ++ settSammen xs

--løsning fra timen, nesten lik
--settSammen [] = []
--settSammen (s:[]) = s
--settSammen (s:strs) = s ++ (' ' : (settSammen strs))

--løsning med list comprehension
settSammen2 :: [String] -> String
settSammen2 (s:ss) = concat (s : [ ' ' : x | x <- ss])

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

--løsning fra timen
delStrengen2 :: String -> [String]
delStrengen2 [] = []
delStrengen2 (' ':ss) = delStrengen2 ss 
delStrengen2 ss = ord : (delStrengen2 rester)
                where 
                    ord = (nesteOrd ss)
                    rester = drop (length ord) ss 

nesteOrd :: String -> String
nesteOrd [] = []
nesteOrd (c:ss) 
                | c == ' ' = []
                | otherwise = c : (nesteOrd ss)

--løsning med takewhile og dropwhile
delStrengen3 :: String -> [String]
delStrengen3 [] = [] 
delStrengen3 (' ':ss) = delStrengen3 ss 
delStrengen3 ss = (takeWhile (/= ' ') ss) : (delStrengen3 (dropWhile (/= ' ') ss))


--løsning med span
delStrengen4 :: String -> [String]
delStrengen4 [] = []
delStrengen4 (' ':ss) = delStrengen4 ss 
delStrengen4 ss = let (ord,rest) = span (/= ' ') ss in 
                    ord : (delStrengen4 rest)


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


--løsning fra timen
gdelStrengen1 :: String -> String -> [String]
gdelStrengen1 ss rs = hjelper_gds1 ss' c
                where 
                    c = head rs
                    ss' = [if or[s == r | r <- rs] then c else s | s <- ss]

hjelper_gds1 :: String -> Char -> [String]
hjelper_gds1 [] _ = []
hjelper_gds1 ss c
                    | (head ss) == c = hjelper_gds1 (tail ss) c
                    | otherwise = let (ord,rest) = span (/= c) ss in 
                        ord : (hjelper_gds1 rest c)


--løsning med foldr
gdelStrengen2 :: String -> String -> [String]
gdelStrengen2 ss rs = foldr func [ss] rs 
                        where 
                            func = \char strs -> concat (map (\str -> hjelper_gds1 str char) strs)