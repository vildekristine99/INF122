import Data.Char
-------Innlevering-------
--1
--data Ast = V Int | P Ast Ast | M Ast Ast 
data Ast = V Int | P Ast Ast | M Ast Ast | B String deriving (Show)
--inn (let (a,r) = parser ["+","1","2"] in a)

eval :: Ast -> Int
eval (V p) = p
eval (P p q) = eval p + eval q
eval (M p q) = eval p * eval q
-- 3.3
eval (B p) = error "Riktig, men kan ikke evalueres"

--2
inn :: Ast -> String
inn (V p) = show p
inn (P p q) = "(" ++ inn p ++ " + " ++ inn q ++ ")"
inn (M p q) = "(" ++ inn p ++ " * " ++ inn q ++ ")"
--inn (B s) = s

--3.1
tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('*':xs) = "*": tokenize xs
tokenize ('+':xs) = "+": tokenize xs
tokenize (x:xs) = if isDigit x 
    then (takeWhile isDigit (x:xs)) : tokenize (dropWhile isDigit xs)
    else (takeWhile isLetter (x:xs)) : tokenize (dropWhile isLetter xs)
{-
--timen
tokenize (x:xs) 
    | isDigit x = let (tall, r) = span isDigit (x:xs) in
        tall : tokenize r
    | isAlpha x = let (ord, r) = span isAlpha (x:xs) in
        ord : tokenize r
    | otherwise = error("Invalid syntax at " ++ [x])
-}

--3.2
--timen
parser :: [String] -> (Ast, [String])
parser [] =  error "Uttrykket er ikke korrekt"
parser ("+":xs) = ((P a1 a2), rest)
                        where 
                            (a1, r1) = parser xs
                            (a2, rest) = parser r1
parser ("*":xs) = ((M a1 a2), rest)
                        where 
                            (a1, r1) = parser xs
                            (a2, rest) = parser r1
parser (x:xs) 
        | isDigit (head x) = ((V (read x :: Int)), xs)
        | otherwise = ((B x), xs)

--min
parseU :: [String] -> (Ast, [String])
parseU [] =  error "Uttrykket er ikke korrekt" --legger til feilmelding til oppgsve 3.3
parseU ("+":xs) = let (e1, r1) = parseU xs;
                      (e2, r2) = parseU r1 in (P e1 e2, r2)
parseU ("*":xs) = let (e1, r1) = parseU xs;
                      (e2, r2) = parseU r1 in (M e1 e2, r2)
parseU (x:xs) = if (onlyDigits x) then (V(read x :: Int), xs)
                    else (B x, xs)
                           
parse :: String -> Ast
parse xs = fst(parseU (tokenize xs))

onlyDigits :: [Char] -> Bool
onlyDigits xs = takeWhile isDigit xs == xs


--3.3
ev :: String -> Int
ev xs = eval (parse xs)

--timen
eve :: String -> Int
eve ss 
        | rest == [] = eval ast
        | otherwise = error ("Could not parse the whole string: "++ concat rest)
        where (ast,rest) = parser(tokenize ss)

--3.4
innfiks :: String -> String
innfiks xs = inn (parse xs)

--innfiks ss = let (ast, rest) = parser (tokenize xs) in
    --inn ast

{-
parseExpr :: [String] -> (Ast, [String])
parseExpr [] =  error "Tom String"
parseExpr ("+":xs) = ((Plus a1 a2), rest)
                        where 
                            (a1, r1) = parseExpr xs
                            (a2, rest) = parseExpr r1
parseExpr ("*":xs) = ((Mult a1 a2), rest)
                        where 
                            (a1, r1) = parseExpr xs
                            (a2, rest) = parseExpr r1
parseExpr ("-":xs) = ((Minus a1 a2), rest)
                        where 
                            (a1, r1) = parseExpr xs
                            (a2, rest) = parseExpr r1
parseExpr (x:xs) 
        | isDigit (head x) = ((Num (read x :: Int)), xs)
        | otherwise = ((Word x), xs)

parseExpr :: [String] -> (Ast, [String])
parseExpr [] =  error "Stringen er tom" --legger til feilmelding til oppgsve 3.3
parseExpr (x:xs)    
    | isDigit (head x) = let (e1, r1) = parseExpr xs in ((Num (read x :: Int)), r1)
    | isAlpha (head x) = let (e1, r1) = parseExpr xs in ((Word x), r1)
    | x == "+" = let (e1, r1) = parseExpr xs;
                     (e2, r2) = parseExpr r1 in (Plus e1 e2, r2)
    | x == "*" = let (e1, r1) = parseExpr xs;
                     (e2, r2) = parseExpr r1 in (Mult e1 e2, r2)
    | x == "-" = let (e1, r1) = parseExpr xs;
                     (e2, r2) = parseExpr r1 in (Minus e1 e2, r2)                        


onlyDigits :: [Char] -> Bool
onlyDigits xs = takeWhile isDigit xs == xs
-}