import Data.Char
--data Ast = V Int | P Ast Ast | M Ast Ast 
data Ast = V Int | P Ast Ast | M Ast Ast | B String deriving (Show)
--1
eval :: Ast -> Int
eval (V p) = p
eval (P p q) = (eval p) + (eval q)
eval (M p q) = (eval p) * (eval q)

--3.3 feilmelding
eval (B p) = error "Riktig, men kan ikke evalueres"

--2
inn :: Ast -> String
inn (V p) = show p 
inn (P p q) = "(" ++ (inn p) ++ " + " ++ (inn q) ++ ")"
inn (M p q) = "(" ++ (inn p) ++ " * " ++ (inn q) ++ ")"

--3.1
tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('*':xs) = "*": tokenize xs
tokenize ('+':xs) = "+": tokenize xs
tokenize (x:xs) = if isDigit x 
    then (takeWhile isDigit (x:xs)) : tokenize (dropWhile isDigit xs)
    else (takeWhile isLetter (x:xs)) : tokenize (dropWhile isLetter xs)


--3.2

parseU :: [String] -> (Ast, [String])
parseU [] =  error "Uttrykket er ikke korrekt" --legger til feilmelding til oppgave 3.3
parseU ("+":xs) = let (e1, r1) = parseU xs;
                      (e2, r2) = parseU r1 in (P e1 e2, r2)
parseU ("*":xs) = let (e1, r1) = parseU xs;
                      (e2, r2) = parseU r1 in (M e1 e2, r2)
parseU (x:xs) = if (onlyDigits x) then (V(read x :: Int), xs)
                    else (B x, xs)
                           
parse :: String -> Ast
parse xs = fst(parseU (tokenize xs))

onlyDigits xs = takeWhile isDigit xs == xs

--3.3
ev :: String -> Int
ev xs = eval (parse xs)

--3.4
innfiks :: String -> String
innfiks xs = inn (parse xs)