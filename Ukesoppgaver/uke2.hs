------------------------INNLEVERING-----------------------------------
--3.11.3
second :: [a] -> a --riktig
second xs = head (tail xs)

swap :: (a,b) -> (b,a) --riktig
swap(x,y) = (y,x)

pair :: a -> b -> (a, b) --riktig
pair x y = (x,y)

--double :: Num a => a -> a
double :: Int -> Int --riktig
double x = x*2

palindrome :: Eq a => [a] -> Bool --riktig
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a --riktig
twice f x = f (f x)

--C
--False :: Bool  riktig
--5 + 8 :: Num a => a / Int  riktig

--(+) 2 :: Num a => a -> a  riktig
-- (+ :: Num a => a -> a -> a)
--(+2) :: Num a => a -> a   riktig
--(2+) :: Num a => a -> a   riktig

--(["foo", "bar"], 'a')  :: ([String], Char) riktig men helst:  ([[Char]], Char)
-- String er en type som er en liste med chars
--[(True, []), (False, [['a']])]  :: [(Bool, [[Char]])]     riktig

-- \x y ->  y !! x  ::  Int -> [a] -> a     riktig
--[ take, drop, \x y -> (y !! x) ]  har ikke type      riktig
--[ take, drop, \x y ->  [ y !! x ] ] :: [Int -> [a] -> [a]]    riktig


--D
-- Alle er ekvivalente utenom foo5
foo1 :: a -> b -> (a,b) --riktig
foo1 x y = (x, y)

foo2 :: a -> b -> (a, b) --riktig
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a, b) --riktig
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b) --riktig
foo4 = \x -> \y -> (x, y)

foo5 :: b -> a -> (a, b) --riktig?
foo5 = \x -> \y -> (y,x)

foo6 :: a -> b -> (a, b) --riktig
foo6 = \y -> \x -> (y,x)


--E
f1 :: a -> (a,a)
f1 x = (x,x)    --riktig

f2 :: (a,b) -> a
f2(x, y) = x    --riktig

f3 :: (a,b) -> b
f3(x, y) = y    --riktig

f4 :: a -> b -> a
f4 x y = x      --riktig

f5 :: a -> b -> b
f5 x y = y      --riktig


--F riktig
h :: Int -> Int -> Int
h x y =  x*y

g :: (Int, Int) -> Int
g (x,y) = x*y

--kan testes med:
--test x y = (f x y) == (g(x,y))