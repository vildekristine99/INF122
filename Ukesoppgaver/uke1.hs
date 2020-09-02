--1.7.4
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, a > x]
                  larger = [b | b <- xs, b <= x]

--1.7.5
--Det vil føre til at om noen er like vil de ikke tas med i resultatlisten.

qsort1 [] = []
qsort1 (x:xs) = qsort1 smaller ++ [x] ++ qsort1 larger
               where
                  smaller = [a | a <- xs, a < x]
                  larger = [b | b <- xs, b > x]
--[1,1,2,3,3] -> [1,2,3]

--2.7.4
--reverse og så head av den resulterende listen.
-- eksempel: head(reverse[1,2,3])
-- svar: 3
-- eksempel2 : last2 = xs !! ((length xs) - 1)

--2.7.5
--reverse(tail(reverse[1,2,3]))
--reverse(drop 1 (reverse[1,2,3]))
--svar: [1,2]


--C
plu :: [Int] -> Int -> [Int]
plu [k] n = [k+n]
plu (k:ks) n = (k+n) : plu ks n


pali :: (Eq a) => [a] -> Bool
pali a = a == reverse a
