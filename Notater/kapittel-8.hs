type Pos = (Int, Int)
data Move = North | South | East | West
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

--8.1
data Nat = Zero | Succ Nat deriving (Show, Read, Eq, Ord)
add :: Nat -> Nat -> Nat 
add Zero n = n 
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

--8.2
--data Ordering = LT | EQ | GT
--compare :: Ord a => a -> a -> Ordering

data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                   = x==y
occurs x (Node l y r) = case compare x y of
                            LT -> occurs x l 
                            EQ -> True
                            GT -> occurs x r
--This version is more efficient because it only requires one comparison between x and y for each node,
--whereas the previous version may require two.

--8.3
--data Tree a = Leaf a | Node (Tree a) (Tree a)

data Ast = V Int | P Ast Ast | M Ast Ast
eval :: Ast -> Int
eval (V x) = x
eval (P x y) = (eval x) + (eval y)
eval (M x y) = (eval x) * (eval y)

inn :: Ast -> String
inn (V x) = show x
inn (P x y) = (inn x) ++ " + " ++ (inn y)
inn (M x y) = (inn x) ++ " * " ++ (inn y)

tokenize' :: String -> [String]
tokenize' [] = []
tokenize' (x:xs)
    | elem x t = [x] : tokenize' xs
    | elem x w = tokenize' xs
    | otherwise = (takeWhile (notion (t++w)) (x:xs)) : tokenize' (dropWhile(notion (t++w)) (x:xs))
        where 
            notion xs = \x -> not(elem x xs)
            t = "*+"
            w = " "