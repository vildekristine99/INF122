data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

size :: Num p => Tree a -> p
size (Leaf x) = 1
size (Node l r) = size l + size r

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node l r) = size l == size r && balanced l && balanced r

mirror :: Tree a -> Tree a
mirror (Leaf x) = Leaf x
mirror (Node l r) = Node (mirror r) (mirror l)

--a
--size (mirror t) = size t
{-
base case:
size (mirror (Leaf x)) 
size (Leaf x) -> by line 12
1 -> by line 4

size (Leaf x) = 1 -> by line 4

Induksjonshypotese:
size (mirror t) = size t

Induksjonssteg:
size (mirror (Node l r)) = size (Node l r)
size (mirror (Node l r))
size (Node (mirror r) (mirror l)) -> by line 13
size (mirror r) + size (mirror l) -> by line 5
size r + size l -> by IH
size l + size r -> kommutative lov
size (Node l r) 

size (Node l r)
size l + size r
-}

-- b
--balanced (mirror t) = balanced t
{-
base case:
balanced (mirror (Leaf x)) = balanced (Leaf x)
balanced (mirror (Leaf x))
balanced (Leaf x) -> by line 12
True -> by line 8

blaanced (Leaf x)
True -> by line 8

Induksjonshypotese:
balanced (mirror t) = balanced t

Induksjonssteg:
balanced (mirror (Node l r)) = balanced (Node l r)
balanced (mirror (Node l r))
balanced (Node (mirror r) (mirror l)) -> by line 13
size (mirror l) == size (mirror r) && balanced (mirror l) && balanced (mirror r) -> by line 9
size l == size r && balanced l && balanced r -> by line 12
balanced (Node l r) -> by line 9

-}

