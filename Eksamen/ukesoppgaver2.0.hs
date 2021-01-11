plu :: [Int] -> Int -> [Int]
plu [] _ = []
plu (k:ks) n = (k+n) : plu ks n 

pali :: Eq a => [a] -> Bool
pali xs = xs == reverse xs

f1 :: a -> (a,a)
f1 x = (x,x)

f2 :: (a,b) -> a
f2 (x,_) = x

rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) k = if x == k then xs else x : rem1 xs k

diff :: Eq a => [a] -> [a] -> [a]
diff xs [] = xs
diff xs (e:els) = diff (rem1 xs e) els


--Korrekthet:
data Tree a = Blad a | Node (Tree a) a (Tree a)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Blad x) = Blad (f x)
tmap f (Node ve x ho) = Node (tmap f ve) (f x) (tmap f ho)

{-
tmap id = id -> ulike typer
tmap (f . g) = (tmap g) . (tmap h)


-}