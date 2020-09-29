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
parseTerm(s) = let (a,z) = parseN(s) in
    if null z then (a,z)
    else if (head(z) == "*") then
        let (c,rest) = parseTerm(tail(z)) in (Mult a c, rest) 
        else  (a,z) --error "forventer *"
parseW :: [String] -> (Ast, [String])
parseW("(":s) = let(a,")":b) = parseExpr(s) in (a,b)
parseW(x:s) = if(isAlpha (head x)) then ((Word x), s) else error "feil1"
parseN :: [String] -> (Ast, [String])
parseN("(":s) = let(a,")":b) = parseExpr(s) in (a,b)
parseN(x:s) = if isDigit (head x) then ((Num (read x :: Int)), s) else error "ulovelig bruk av mult"
    


parse :: String -> Ast
parse str = fst(parseExpr(tokenize str))
