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
{-
Expr -> Term + Expr | Term - Expr | Term
Term -> Number * Term | Word
Word -> Letter Word | Letter
Number -> Digit Number | Digit
Letter -> a | b | c | ...| x | y | z | A | B | ...| Y | Z
Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-}
-- && (isDigit(head(show a))
 {-
 parseExpr :: [String] -> (Ast, [String])
parseExpr(s) = let (a,z) = parseP(s) in 
    if null z then (a,z)
    else if head(z) == "-" then
        let (c,rest) = parseExpr(tail(z)) in (Minus a c, rest)
        else  (a,z)--error "forventer +"
parseTerm :: [String] -> (Ast, [String])
parseTerm(s) = let (a,z) = parseF(s) in
    if null z then (a,z)
    else if (head(z) == "*") && (isDigit(head(show a))) then
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
 -}
se (Word a) = a
se (Num a) = show a

--1
parseExpr :: [String] -> (Ast, [String])
parseExpr(s) = let (a,z) = parseTerm(s) in 
    if null z then (a,z)
    else if head(z) == "-" then
        let (c,rest) = parseExpr(tail(z)) in (Minus a c, rest)
    else if head(z) == "+" then
        let (c,rest) = parseExpr(tail(z)) in (Plus a c, rest)
        else  (a,z) --error "forventer -"
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
parseE :: [String] -> (Ast, [String])
parseE("(":s) = let(a,")":b) = parseExpr(s) in (a,b)
parseE(x:s) | isAlpha (head x) = ((Word x), s) 
            | isDigit (head x) = ((Num (read x :: Int)), s)

    

onlyDigits :: [Char] -> Bool
onlyDigits xs = takeWhile isDigit xs == xs

onlyAlpha :: [Char] -> Bool
onlyAlpha xs = takeWhile isAlpha xs == xs


parse :: String -> Ast
parse str = fst(parseExpr(tokenize str))
