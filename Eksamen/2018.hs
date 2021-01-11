-----------1 Programmer følgende funksjoner:----------
--1.1
mengde :: Eq t => [t] -> Bool
mengde [] = True
mengde (x:xs) = notElem x xs && mengde xs

--1.2
rep :: Eq t => [t] -> [t]
rep [] = []
rep (x:xs) | x `elem` xs = rep xs
           | otherwise = x:rep xs

--1.3
del :: Eq t => [t] -> [t] -> Bool
del [] _ = True
del (x:xs) ys = elem x ys && del xs ys

--1.4
eq :: Eq t => [t] -> [t] -> Bool 
eq xs ys = del xs ys && del ys xs

--1.5
eqG :: (t -> t -> Bool) -> [t] -> [t] -> Bool
eqG _ _ [] = True
eqG _ [] _ = True
eqG pr (x:xs) (y:ys) = (or [pr x y | y <- y:ys] && eqG pr (y:ys) xs) && (or [pr y x | x <- x:xs] && eqG pr ys (x:xs))

--1.6
ps :: Eq a => [a] -> [[a]]
ps xs = reverse (p (rep xs))

p :: [t] -> [[t]]
p [] = [[]]
p (x:xs) = map (x:) (p xs) ++ p xs

{-
ps [] = [[]],
ps [1,2] = [[],[1],[2],[1,2]] = ps [1,2,1]
ps "abc" = ["","a","b","c","ab","ac","bc","abc"] = ps "baabbcc"
-}


----------2 Rettede grafer---------------

kL :: [(Char, Char)]
kL = [('a', 'c'),('b', 'a'),('c', 'b'),('d', 'c'),('d', 'e')]

nL :: [(Char, [Char])]
nL = [('a',['c']),('b',['a']),('c',['b']),('d',['c','e']),('e',[])]


--2.1
snuK :: [(t,t)] -> [(t,t)]
snuK [] = []
snuK ((x,y):xs) = [(y,x)] ++ snuK xs

--2.2 
--snuN :: [(t,[t])] -> [(t,[t])]
snuN :: [(Char, [Char])] -> [(Char, [Char])]
snuN [] = []
snuN ((x,_):ls) = (x, finnNabo x (snuK kL)) : snuN ls

finnNabo :: Eq t => t -> [(t, a)] -> [a]
finnNabo _ [] = []
finnNabo p ((x,y):xs) = if x == p then y:finnNabo p xs else finnNabo p xs


--fasit
snuN' :: Eq t => [(t, [t])] -> [(t, [t])]
snuN' xs = let kL = convNK xs in map (collectOne kL) (noder kL)

convNK :: [(b, [a])] -> [(a, b)]
convNK nl = concat (map oneN nl)

oneN :: (b, [a]) -> [(a, b)]
oneN (a,ns) = map (\x -> (x,a)) ns

noder :: Eq t => [(t, t)] -> [t]
noder kls = rep ([x | (x,z) <- kls] ++ [z | (x,z) <- kls])

collectOne :: Eq a1 => [(a1, a2)] -> a1 -> (a1, [a2])
collectOne kl x = (x,[z | (y,z) <- kl, y==x])

--2.3
--reach :: [(t,[t])] -> t -> [t]
reach [] node = [node]
reach ((x,xs):ls) node = if node `elem` xs then x:reach ls node ++ reach ls x else reach ls node
--ikke ferdig


---------------3 Litt IO-------------------
main :: IO()
main = do
    input <- getLine
    let cmd = words input
    case cmd of
        [] -> do return()
        (x:xs) -> do 
            if x == "L" then do 
                innhold <- readFile (head xs)
                delmengder (words innhold)
                main
            else do
                delmengder (x:xs)
                main
            


delmengder :: [String] -> IO()
delmengder xs = if mengde xs then mapM_ print (ps xs) else putStrLn "Ikke delmengde"




---------------4 Resonnering om programmer---------------------

--4.1
{-
Base case: 
snuK(snuK [])
snuK [] -> by line 53
[] -> by line 53

Induksjonshypotese:
snuK(snuK ks) = ks

Induksjonssteg:
snuK(snuK (x,y):ks) = (x,y):ks
snuK(snuK (x,y) ++ ks) = (x,y):ks
snuK(snuK ks ++ snu(x,y)) = (x,y):ks -->distribution
snuK(snuK ks ++ [(y,x)]) =
snuK([(y,x)]) ++ snuK(snuK ks) = --> distribution
[(x,y)] ++ ks = (x,y):ks --> IH

Konklusjon: 
Overnfor vises det at for en hver kantliste ks gjelder likheten snuK(snuK ks)==ks
Proposisjonen er sann for listen = [] og listen = ks tilsier listen = x:xs
derfor, med induksjon finner vi at propsisjonen er sann for alle kantlister ks
-}

--4.2
mengde :: Eq t => [t] -> Bool
mengde [] = True
mengde (x:xs) = notElem x xs && mengde xs

rep :: Eq t => [t] -> [t]
rep [] = []
rep (x:xs) = if x `elem` xs then rep xs else x:rep xs

Base case:
mengde(rep [])
mengde [] -> by line 147
True -> by line 143

Induksjonshypotese:
mengde(rep xs) = True 

Induksjonssteg:
mengde(rep (x:xs)) = True
mengde(rep (x:xs))
mengde(if x `elem` xs then rep xs else x:rep xs) -> line 148
if x `elem` xs then mengde(rep xs) else mengde(x:rep xs) 
if x `elem` xs then True else mengde(x:rep xs)  -> IH
Må bevise at: mengde(x:rep xs) også er True
mengde(x:rep xs)
notElem x (rep xs) && mengde(rep xs) -> line 144
notElem x (rep xs) && True -> IH
må vise at denne er sann: notElem x (rep xs)
notElem x (rep xs)
notElem x (rep y:xs)
notElem x (if y `elem` xs then rep xs else y:rep xs)
if y `elem` xs then notElem x (rep xs) else notElem x (y:rep xs)
Dette gir to utfall:
y `elem` xs sann og usann:
y `elem` xs = True: 
notElem x (rep xs)

notElem x (rep xs) = False:
notElem x (y:rep xs)








