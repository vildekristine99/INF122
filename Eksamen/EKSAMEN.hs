import Data.Char
--Kandidatnummer: 178
----------1 Evaluering------------
--1.1
{-
foldl (-) 0 [2,4,6]
(0 - 2) = -2
-2 - 4 = -6
-6 - 6 = -12

((0 - 2) - 4) - 6
-}

--1.2
{-
foldr (-) 0 [2,4,6]
6 - 0 = 6
4 - 6 = -2
2 - (-2) = 4

2 - (4 - (6 - 0))
-}

---------2 Noen liste funksjoner------------
--2.1
funk :: Eq a => [a] -> Bool
funk xs = hjelp [x | (x,y) <- zip xs [0..], even y]

hjelp :: Eq a => [a] -> Bool
hjelp [] = True
hjelp (x:xs) | x `elem` xs = False
             | otherwise = hjelp xs

--2.2
--har ikke kjørt den så kan være feil mtp. å fjerne om det er ett elem.
avb::Eq a => [a]->[a]->[a]
avb str [l] = [x | x <- str , x /= l] 
avb str ls | not (funk ls) = str
           | otherwise = replace str ls 


replace :: Eq a => [a] -> [a] -> [a]
replace str [] = str
replace str (x:y:ls) = replace (erstatt str x y) ls


erstatt :: Eq a => [a] -> a -> a -> [a]
erstatt [s] _ _= [s]
erstatt (s:str) x y 
    | x == s = y : erstatt str x y
    | otherwise = s : erstatt str x y                    


--2.3
fle :: Eq a => [a]->[a]->[a]
fle [] ys = ys
fle xs [] = xs
fle (x:xs) (y:ys) = x : y : fle xs ys

--2.4
bin :: (Int->Int->Int) -> String -> String -> String
bin f x y = show (f (read x) (read y))

--2.5
umin :: String->String
umin xs | head xs == '-' = tail xs
        | otherwise = '-' : xs


----------3 Direktete evaluering av prefiks uttrykk--------------
{-
E ::= Tall|Ord|* E E |+ E E |– E
Tall ::= Digit|Digit Tall
Ord ::= Bokstav|Bokstav Ord
Digit::= 0|1|...  8|9
Bokstav::= a|A|b|B ...  z|Z

E ::= Tall|* E E |+ E E |– E
Tall ::= Digit|Digit Tall
Digit::= 0|1|...  8|9
-}
--"+abc*2 3+17 13"
--3.1
tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('*':xs) = "*": tokenize xs
tokenize ('+':xs) = "+": tokenize xs
tokenize ('-':xs) = "-": tokenize xs
tokenize (x:xs) = if isDigit x 
    then takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs)
    else tokenize xs

eva :: String -> (String, String)
eva xs = (show (fst (parseE (tokenize xs))), concat (snd (parseE (tokenize xs))))

parseE :: [String] -> (Int, [String])
parseE [] = (0, [])
parseE ("*":xs) = let (e1, r1) = parseE xs; (e2, r2) = parseE r1 in (e1 * e2, r2)
parseE ("+":xs) = let (e1, r1) = parseE xs; (e2, r2) = parseE r1 in (e1 + e2, r2)
parseE ("-":xs) = let (e1, r1) = parseE xs; (_, r2) = parseE r1 in (-e1, r2)
parseE (x:xs) = if all isDigit x then (read x, xs)
                    else error ("Syntaksfeil ved " ++ x)

--3.2
{-
evg :: [String] -> (String->String->String)
              -> (String->String->String)
              -> (String->String)
              -> (String->String) -> (Int,[String])
--evg [] _ _ _ = (0, [])
evg ("*":xs) g p m = let (e1, r1) = evg xs g p m; (e2, r2) = evg r1 g p m in (g e1 e2, r2)
evg ("+":xs) g p m = let (e1, r1) = evg xs g p m; (e2, r2) = evg r1 g p m in (p e1 e2, r2)
evg ("-":xs) g p m = let (e1, r1) = evg xs g p m; (_, r2) = evg r1 g p m in (m e1, r2)
evg (x:xs) g p m = if all isDigit x then (read x, xs)
                    else error ("Syntaksfeil ved " ++ x)
-}


--3.3
--eva = evg xs (*) (+) (umin ze)

--3.4
{-
fst (evg "- + xxxxx * abcd abd" avb fle reverse id)
- ("xxxxx") + ("abcd" * "abd")
reverse (fle ("xxxxx") (avb "abcd" "abd"))
reverse (fle ("xxxxx") ("bbc")
reverse "xbxbxcxx"
"xxcxbxbx"
-}

-----------4 Lambda uttrykk------------
f :: (Eq p, Num p) => p -> p -> p
f x 0 = 0
f x y = (x * y) + f x (y-1)

g :: Integer -> Integer -> Integer
g = \x -> \y -> if y == 0 then 0  else (x*y) + g x (y-1)


-----------5 Typeavledning-------------
{-
E(Ø | \x -> x (\y -> \z -> y z) :: t)
t4 => {t=a->b} U E({x::a} | x (\y -> \z -> y z) :: b)
t3 => {t=a->b} U E({x::a} | (\y -> \z -> y z) :: c)
               U E({x::a} | x :: c -> b)
t2 => {t=a->b, a=c->b} U E({x::a} | (\y -> \z -> y z) :: c)
t4 => {t=a->b, a=c->b, c=d->e} U E({x::a, y::d} | (\z -> y z) :: e)
t4 => {t=a->b, a=c->b, c=d->e, e=f->g} U E({x::a, y::d, z::f} | y z :: g)
t3 => {t=a->b, a=c->b, c=d->e, e=f->g} 
                U E({x::a, y::d, z::f} | z :: h)
                U E({x::a, y::d, z::f} | y :: h -> g)
t2 => {t=a->b, a=c->b, c=d->e, e=f->g, f=h, d=h->g}

{t=a->b, a=c->b, c=d->e, e=f->g, f=h, d=h->g}
{t=a->b, a=c->b, c=(h->g)->e, e=f->g, f=h, d=h->g}
{t=a->b, a=c->b, c=(h->g)->e, e=h->g, f=h, d=h->g}
{t=a->b, a=c->b, c=(h->g)->(h->g), e=h->g, f=h, d=h->g}
{t=a->b, a=((h->g)->(h->g))->b, c=(h->g)->(h->g), e=h->g, f=h, d=h->g}
{t=(((h->g)->(h->g))->b)->b, a=((h->g)->(h->g))->b, c=(h->g)->(h->g), e=h->g, f=h, d=h->g}

t=((h->g)->(h->g)->b)->b

svar: \x -> x (\y -> \z -> y z) :: ((h->g)->(h->g)->b)->b
uttrykket har en type!
-}


-----------6 Korrekthet---------------
{-
(++)::[a]->[a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

filter :: (a->Bool)->[a]->[a]
filter p [] = []
filter p (x:xs) = if (p x) then x:(filter p xs) else (filter p xs)
-}

--if b then g(u1) else g(u2) = g(if b then u1 else u2)
{-
base case: 
filter p ([] ++ ys) = filter p ys = [] ++ filter p ys -> by line 112
filter p [] ++ filter p ys = [] ++ filter p ys -> by line 117

Induksjonshypotese (IH): 
filter p (xs ++ ys) = filter p xs ++ filter p ys

Induksjonssteg:
filter p ((x:xs) ++ ys) = filter p (x:xs) ++ filter p ys
filter p x:(xs ++ ys) -> by line 114 
if (p x) then x:(filter p (xs ++ ys)) else (filter p (xs ++ ys)) -> by line 118

p x = False:
filter p (xs ++ ys)
filter p xs ++ filter p ys -> IH

p x = True:
x:(filter p (xs++ys)) 
x:(filter p xs ++ filter p ys) ->IH
x:filter p xs ++ filter p ys -> 114

filter p (if p x then x:(xs) else (xs))


filter p (if p x then x:(xs) else (xs))

p x = True:
filter p (xs++ys)
if (p x) then head x:(filter p (tail xs ++ ys)) else (filter p (xs ++ ys)) -> by line 118


-}