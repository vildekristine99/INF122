--------1 Velg riktig svar--------
{-
1.1 filter even (map (*2) [1..5])
[2,4,6,8,10]
d

1.2 evaluer: take 5 nats
a) [0,1,1,1,1]
b) [0, siden tail ikke terminerer, må ha mer enn ett element
c) [0,1,2,3,4]
d) [1,2,3,4,5]

1.3 concat ["ab", "cd", "", "efg"]
a) concat xss = [x | x <- xss] => ["ab", "cd", "", "efg"]
b) concat xss = [x | xs <- xss, x <- xs] => "abcdefg"
c) concat xss = concat (tail xss) => tail av [] og programmet henger
d) concat xss = map (++) xss => går ikke, kan ikke vises

1.4 apply f x = f x
b) typen er (a->b)->a->b

-}


---------2 Matrisemultipliksjon----------------
--2.1
--row m r = r-te rad
--row [[1,2],[3,4]] 2 = [3,4]
import Data.Char
import Data.List
row :: [[Int]] -> Int -> [Int]
row m r = if length m >= r then m !! (r-1) else error "Utenfor matrisen "

--2.2
--col m k = k-te kolonne
--col [[1,2],[3,4]] 1 = [1,3]
col :: [[Int]] -> Int -> [Int]
col m k = map (\x -> x !! (k-1)) m --kunne skrevet (!!(k-1))

--2.3
--cols rad = kols
--cols [[1,2],[3,4]] = [[1,3],[2,4]]
cols :: [[Int]] -> [[Int]]
cols mr = map (col mr) [1..length mr] 
--transpose

--2.4
--mult [[1,2],[3,4]] [[3,4],[5,6]] = [[13,16],[29,36]]
mult :: [[Int]] -> [[Int]] -> [[Int]]
mult ma mb = [map (\c -> sum [x*y | (x,y) <- zip (row ma i) c]) (cols mb) | i <- [1..length ma] ]

--multrc r c = sum [x*y | (x,y) <- zip r c]
--mult ma mb = [map (multrc (row ma x)) (cols mb) | x <- [1..length ma]]


-----------3 IO og postfiksuttrykk----------------
{-
E ::= Pos | E E * | E E + | E E –
Pos ::= Digit | DigitPos
Digit ::= 0 | 1 | ... 8 | 9
-}

eval :: IO()
eval = hjelp []

--hjelp :: [String] -> IO ()
hjelp :: [Int] -> IO ()
hjelp xs = do
    text <- getLine
    let input = dropWhileEnd isSpace text
    if all isDigit input then do
        let list = (read input :: Int):xs
        print list
        hjelp list
    else if length xs > 2 then do
        case input of
            ['*'] -> do
                    let list = ((xs !! 1) * head xs):tail(tail xs)
                    print list
                    hjelp list
            ['+'] -> do 
                    let list = ((xs !! 1) + head xs):tail(tail xs)
                    print list
                    hjelp list
            ['-'] -> do
                    let list = ((xs !! 1) - head xs):tail(tail xs)
                    print list
                    hjelp list
    else if length xs < 2 then do
        putStrLn "Ikke nok elementer i listen"
        hjelp xs
    else do
        putStrLn ("Ugyldig tegn " ++ show input)
        hjelp xs


--------------4 Hindley-Milner----------------
--apply f x = f x 
--4.1 
apply' :: (t1 -> t2) -> t1 -> t2
apply' = \f -> \x -> f x

--4.2
--MÅ LÆRE OSS HINDLET MILNER

