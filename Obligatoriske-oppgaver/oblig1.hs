-- Vilde Kristine Fossum, Gruppe 1
module Oblig1 where
import Data.Char


data Ast
    = Word String
    | Num Int
    | Mult Ast Ast
    | Plus Ast Ast
    | Minus Ast Ast
    deriving (Eq, Show)

tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('*':xs) = "*": tokenize xs
tokenize ('+':xs) = "+": tokenize xs
tokenize ('-':xs) = "-": tokenize xs
tokenize (x:xs) = if isDigit x 
    then (takeWhile isDigit (x:xs)) : tokenize (dropWhile isDigit xs)
    else (takeWhile isLetter (x:xs)) : tokenize (dropWhile isLetter xs)

--1
parseExpr :: [String] -> (Ast, [String])
parseExpr(s) = let (a,z) = parseP(s) in 
    if null z then (a,z)
    else if head(z) == "-" then
        let (c,rest) = parseExpr(tail(z)) in (Minus a c, rest)
        else  (a,z)--error "forventer +"
parseTerm :: [String] -> (Ast, [String])
parseTerm(s) = let (a,z) = parseF(s) in
    if null z then (a,z)
    else if (head(z) == "*") then
        let (c,rest) = parseTerm(tail(z)) in (Mult a c, rest) 
        else  (a,z) --error "forventer *"
parseP :: [String] -> (Ast, [String])
parseP(s) = let (a,z) = parseTerm(s) in
    if null z then (a,z)
    else if head(z) == "+" then
        let (c,rest) = parseP(tail(z)) in (Plus a c, rest)
        else  (a,z) --error "forventer -"
parseF :: [String] -> (Ast, [String])
parseF("(":s) = let(a,")":b) = parseExpr(s) in (a,b)
parseF(x:s) 
    | isDigit (head x) = ((Num (read x :: Int)), s)
    | isAlpha (head x) = ((Word x), s)


parse :: String -> Ast
parse str = fst(parseExpr(tokenize str))

--har prøvd å gjøre parseren slik at den ikke godtar at man avslutter på et nummer
--men får den ikke til å funke
{-
parseTerm :: [String] -> (Ast, [String])
parseTerm(s) = let (a,z) = parseE(s) in
    if ((null z) && not (onlyDigits(show (a)))) 
        then (a,z)
    else 
        if onlyDigits(show (a)) then
            if((head(z) == "*") && onlyAlpha(last z)) then
                let (c,rest) = parseTerm(tail(z)) in (Mult a c, rest) 
            else  error "forventer *"
        else let (c,rest) = parseE(tail(z)) in (Mult a c, rest)
-}


--2
space :: String
space = "   "

tre :: Ast -> String -> String
tre (Word w) s = s ++ "Word " ++ w ++ "\n"
tre (Num n) s = s ++ "Num " ++ show n ++ "\n"
tre (Mult x y) s = s ++ "Mult\n" ++ tre x (s ++ space) ++ tre y (s ++ space)
tre (Plus x y) s = s ++ "Plus\n" ++ tre x (s ++ space) ++ tre y (s ++ space)
tre (Minus x y) s = s ++ "Minus\n" ++ tre x (s ++ space) ++ tre y (s ++ space)

viss :: Ast -> String
viss ast = tre ast ""

vis :: Ast -> IO ()
vis ast = putStr (viss ast)

{-
Expr -> Term + Expr | Term - Expr | Term
Term -> Number * Term | Word
Word -> Letter Word | Letter
Number -> Digit Number | Digit
Letter -> a | b | c | ...| x | y | z | A | B | ...| Y | Z
Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-}
--3
ev :: Ast -> String
ev (Word w) = w
ev (Num n) = show n
ev (Mult (Num x) y) = concat (replicate (read (show x) :: Int) (ev y))
-- otherwise = error "ikke gyldig"
ev (Plus x y) 
    | onlyAlpha (ev x) && onlyAlpha (ev y) = ev x ++ ev y
    | otherwise = error "ikke gyldig"
ev (Minus x y) 
    | onlyAlpha (ev x) && onlyAlpha (ev y) = diff (ev x) (ev y)
    | otherwise = error "ikke gyldig"

-- | onlyDigits (ev x) && onlyDigits (ev y) = show ((read (ev x) :: Int) - (read (ev y) :: Int))
-- | onlyDigits (ev x) && onlyDigits (ev y) = show ((read (ev x) :: Int) + (read (ev y) :: Int))  
-- | onlyDigits (show x) && onlyDigits (ev y) = show ((read (show x) :: Int) * (read (ev y) :: Int))
-- | onlyDigits (show x) && onlyAlpha (ev y) = 

eval :: Ast -> String
eval str = ev str

onlyDigits :: [Char] -> Bool
onlyDigits xs = takeWhile isDigit xs == xs

onlyAlpha :: [Char] -> Bool
onlyAlpha xs = takeWhile isAlpha xs == xs


--Til og trekke fra en string fra en anna string
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) el = if (x == el) 
                    then xs
                    else x : (rem1 xs el)
                    
diff :: Eq a => [a] -> [a] -> [a]
diff xs [] = xs
diff xs (e:els) = diff (rem1 xs e) els

