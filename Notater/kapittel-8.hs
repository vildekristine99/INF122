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
data Ordering = LT | EQ | GT
--compare :: Ord a => a -> a -> Ordering


data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)                   = x==y
occurs x (Node l y r)   | x == y    = True
                        | x < y     = occurs x l 
                        | otherwise = occurs x r
