--------------Problem 1-------------------

--a
{-
[ ( ) ] ( ) ,
( ( ) ) ( [ ] ) ,
( ( ) ) [ [ ] ] ( ( ) [ ] ) , ...
-}
--not
{-
] ( ) [ ( ) ,
( [ ) ] ( [ ] ) , ...
-}

{-
A -> (B) | [B] | A A
B -> A | tom
-}

--b
--se notat på ipad


---------------Problem 2--------------------

--a
foldr' :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

--append [4,5,6] [1,2,3] -> [4,5,6,1,2,3]
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs 

--b
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl f (f v x) xs

-- foldl' (\ x -> \ y -> x/2 + y) 4 [2,4,6]
-- 4/2 + 2 = 4
-- 4/2 + 4 = 6
-- 6/2 + 6 = 9
-- svar: 9


---------------Problem 3----------------------
type Name = String
type Pnum = Integer
type Pbook = Name -> Maybe Pnum

lookup :: Pbook -> Name -> Maybe Pnum
lookup b n = b n
