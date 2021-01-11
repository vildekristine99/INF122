------------1 Programmer følgende funkajsoner-----------------
--1.1
harEl :: (t -> Bool) -> [t] -> Bool
harEl _ [] = False
harEl pr xs | not (pr (head xs)) = harEl pr (tail xs)
            | otherwise = True

--1.2
el :: (t -> Bool) -> [t] -> t
el pr xs    | pr (head xs) = head xs
            | otherwise = el pr (tail xs)

--1.3
gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep _ _ [] = []
gRep pr y (x:xs)    | pr x = y : gRep pr y xs
                    | otherwise = x : gRep pr y xs

gRep2 pr y = map (\x -> if pr x then y else x)

--1.4
data BT = B Int | N BT Int BT

--a
elt :: BT -> Int -> Bool 
elt (B p) x = p == x
elt (N b1 n b2) x = (n == x) || elt b1 x || elt b2 x

--b
toL :: BT -> [Int]
toL (B p) = [p]
toL (N b1 n b2) = toL b1 ++ [n] ++ toL b2

--c
dup :: BT -> Bool
dup tr = hjelp (toL tr)

hjelp :: Eq a => [a] -> Bool
hjelp [] = False
hjelp (x:xs) = elem x xs || hjelp xs

-----------2 Rettede grafer-------------
--noder = [a, b, c, d, e]
--KL = [(a, b),(a, c),(b, d),(c, b),(d, c),(d, e)]
--NL = [(a, [b, c]),(b, [d]),(c, [b]),(d, [c, e]),(e, [])]
nL = [('a',['c']),('b',['a']),('c',['b']),('d',['c','e']),('e',[])]

--2.1
kL :: [(Char, Char)]
kL = [('a', 'c'),('b', 'a'),('c', 'b'),('d', 'c'),('d', 'e')]

naboL :: Eq t => [(t,t)] -> [(t,[t])]
naboL [] = []
naboL ((x,_):ls) = (x, findN ls x) : naboL ls

findN :: Eq t => [(t, a)] -> t -> [a]
findN [] _ = []
findN ((x,y):ls) z = if x == z then y:findN ls z else findN ls z

--2.2
kantL :: [(t, [t])] -> [(t,t)]
kantL = foldr (\(f, nl) ak -> [(f,n)| n <- nl] ++ ak) [] 
