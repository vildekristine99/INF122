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
tokenize (x:xs) 
    | isDigit x = let (tall, r) = span isDigit (x:xs) in
        tall : tokenize r
    | isAlpha x = let (ord, r) = span isAlpha (x:xs) in
        ord : tokenize r
    | otherwise = error("Invalid syntax at " ++ [x]) --viktig for at den skal fungere!


parse :: String -> Ast
parse str 
    | null r = a
    | otherwise = error "Coundnt parse string"  --viktig, lite alvorlig men feil. om det ikke er oppgitt vil man måtte ta hensyn til feil
    where (a,r) = parseExpr(tokenize str)

parseExpr :: [String] -> (Ast, [String])
parseExpr [] = error "Missing expr"
parseExpr strs
    | null r1 = (a1, r1)
    | head r1 == "+" = let (a2,r2) = parseExpr (tail r1) in
                        ((Plus a1 a2), r2)
    | head r1 == "-" = let (a2,r2) = parseExpr (tail r1) in
                        ((Minus a1 a2), r2)
    | otherwise = error ("Invalid sybtax at :" ++ (head r1))
    where (a1, r1) = parseTerm strs

parseTerm :: [String] -> (Ast, [String])
parseTerm [] = error "Missing Term"
parseTerm [s]
    | isDigit (head s) = error "Term cant end with number"
    | isAlpha (head s) = ((Word s), [])
    | otherwise = error ("Cant end with " ++ s)
parseTerm (s:strs) 
    | isDigit (head s) = if (head strs) == "*" 
                            then let (t, r) = parseTerm (tail strs) in
                                    ((Mult (Num (read s :: Int)) t), r)
                            else error "Num must preceed *"
    | isAlpha (head s) = if (head strs) /= "*"
                            then ((Word s), strs)
                            else error "Cant multiply strings"
    | otherwise = error "Not valid term"


viss :: Ast -> String
viss a = viss' a 0

viss' :: Ast -> Int -> String
viss' (Word s) n = spaces n ++ "Word " ++ s ++ "\n"
viss' (Num x) n = spaces n ++ "Num " ++ show x ++ "\n"
viss' (Mult a1 a2) n = spaces n ++ "Mult \n" ++ (viss' a1 (n+1)) ++ (viss' a2 (n+1))
viss' (Plus a1 a2) n = spaces n ++ "Plus \n" ++ (viss' a1 (n+1)) ++ (viss' a2 (n+1))
viss' (Minus a1 a2) n = spaces n ++ "Minus \n" ++ (viss' a1 (n+1)) ++ (viss' a2 (n+1))

spaces :: Int -> String
--spaces n = concat ["   " | _ <- [1..n]]
spaces n = concat (replicate n "   ") 

vis :: Ast -> IO ()
vis ast = putStr (viss ast)


eval :: Ast -> String 
eval (Word s) = s
eval (Mult (Num n) a) = concat(replicate n (eval a))
eval (Mult _ _) = error "Left of mult must be num"
eval (Plus a1 a2) = (eval a1) ++ (eval a2)
eval (Minus a1 a2) = diff (eval a1) (eval a2)
eval a = error "Invalid ast"
--alltid har med en default case når du skriver pattern matching!



rem1 :: String -> Char -> String
rem1 [] _ = []
rem1 (x:xs) el = if (x == el) 
                    then xs
                    else x : (rem1 xs el)
                    
diff :: String -> String -> String
diff xs els = foldl rem1 xs els