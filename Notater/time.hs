flett :: Ord a => [a] -> [a] -> [a]

flett [] ys = ys
flett xs [] = xs

flett (x:xs) (y:ys) = if x < y then x:(flett xs (y:ys)) else y:(flett (x:xs) ys)
