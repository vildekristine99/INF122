----- Innlevering -----
--7.1
comp :: (t -> a) -> (t -> Bool) -> [t] -> [a]
comp f p xs = [f x | x <- xs, p x]

comp' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comp' f p xs = map (f) (filter p xs)

--timen, egentlig helt likt
listComp :: [a] -> (a -> Bool) -> (a -> b) -> [b]
listComp xs p f = [f x | x <- xs, p x]

higherOrder :: [a] -> (a -> Bool) -> (a -> b) -> [b]
higherOrder xs p f = map (f) (filter p xs)

--7.4
dec2int :: [Int] -> Int
dec2int = foldl lagTall 0
-- dec2int [2,3,4,5]
-- 2345
lagTall :: Int -> Int -> Int
lagTall = (\x y -> 10*x + y)

--timen show og read
dec2int' :: [Int] -> Int
dec2int' ns = read (foldl func "" ns) :: Int
        where func = \ls n -> ls ++ (show n)

--timen math way
dec2int'2 :: [Int] -> Int
dec2int'2 ns = foldl func 0 ns
        where func = \sum n -> 10*sum + n


--7.5
curry' :: ((a, b) -> t) -> a -> b -> t
curry' f = (\x y -> f (x,y))

--timen
curry2 :: ((x, y) -> result) -> x -> y -> result
curry2 func = \param1 -> \param2 -> func (param1, param2)
pairFunc (x,y) = x+y

uncurry' :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry' f = (\(x, y) -> f x y)

--timen
uncurry2 :: (x ->y -> result) -> (x, y) -> result
uncurry2 func = \pair -> let (p1, p2) = pair in func p1 p2
curriedFunc x y  = x + y


--7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
-- glemte denne:
altMap f _ (x:[]) = f x : []
altMap f g (x:y:[]) = f x : g y : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs
    
{-
altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]
-}
--timen
altMap2 :: (t -> b) -> (t -> b) -> [t] -> [b]
altMap2 f1 f2 ls = map func (zip ls [0..])
                where func = \(elem, idx) -> if idx `mod` 2 == 0
                                                then f1 elem
                                                else f2 elem

altMap3 :: (t -> a) -> (t -> a) -> [t] -> [a]
altMap3 f1 f2 ls = concat $ map func (pairing ls)
        where 
            func [x,y] = [f1 x, f2 y]
            func [x] = [f1 x]
            pairing (x:y:ls) = [x,y] : pairing ls
            pairing (x:ls) = [x]:[]
            pairing [] = []

--8.5
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val v) = f v
folde f g (Add w v) = g (folde f g w) (folde f g v)

--8.6
eval :: Expr -> Int
eval x = folde (samme) (+) (x)

samme :: p -> p
samme x = x

--timen
eval' :: Expr -> Int
eval' e = folde (id) (+) (e)--folde fEval gEval e

fEval :: Int -> Int
fEval = \n -> n --id
gEval :: Int -> Int -> Int
gEval = \n1 n2 -> n1+n2 -- +



size :: Expr -> Int
size x = folde (count) (+) x

count :: Num p1 => p2 -> p1
count _ = 1  

--lik count
fSize :: Int -> Int
fSize = \_ -> 1 -- (const 1)


--B
infiks     :: Expr -> String
infiks (Val p) = show p
infiks (Add p q) = "(" ++ infiks p ++ " + " ++ infiks q ++ ")"

--timen
infiks' :: Expr -> String
infiks' e = init $ tail $ folde show gInfiks e

gInfiks :: [Char] -> [Char] -> [Char]
gInfiks = \s1 s2 -> "(" ++ s1 ++ " + " ++ s2 ++ ")"


prefiks  :: Expr -> String
prefiks (Val p) = show p
prefiks (Add p q) = "+ " ++ prefiks p ++ " " ++ prefiks q

--timen
prefiks' :: Expr -> String
prefiks' e = folde show gPrefiks e

gPrefiks :: [Char] -> [Char] -> [Char]
gPrefiks = \s1 s2 -> "+ " ++ s1 ++ " " ++ s2


postfiks :: Expr -> String
postfiks (Val p) = show p
postfiks (Add p q) = postfiks p ++ " " ++ postfiks q ++ " +"

--timen
postfiks' :: Expr -> String
postfiks' e = folde show gPostfiks e

gPostfiks :: [Char] -> [Char] -> [Char]
gPostfiks = \s1 s2 -> s1 ++ " " ++ s2 ++ "+ " 