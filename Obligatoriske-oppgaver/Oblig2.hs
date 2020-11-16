{-# LANGUAGE LambdaCase #-}
import Data.Char
import System.Directory ( doesFileExist )

--Vilde Kristine Fossum, Gruppe 1

--Slider from 2.4: 10 (s 0 0, b 2 2) 2 10 3 9 4 9 5 10

-- valgte å tolke dit hen at c og r bare skal kunne gjøres ved starten.
-- brukt gjennomgang fra timene som inpirasjon

type Pos = (Int,Int)
type Board = [Pos]

main :: IO()
main = do 
    clrInput
    command <- getLine -- Get commando
    let cs = words command-- Split on spaces
    case cs of -- Check comando
        ("quit":[]) -> return () -- quit
        ("c":x:[]) -> do  
                        if (allNumbers [x]) 
                            then do 
                                c (read x)
                                gameLoop (read x) [] [3,3] [2,3]
                            else do putStrLn "ulovelig input" 
                                    main
        ("r":filnavn:[]) -> do 
            -- 5 (s 2 3, b 3 3) 3 3 1 3 2 1 3 2 2 3 
                            let check = doesFileExist (filnavn)
                            check >>= \case
                                True ->
                                        do
                                        filInnhold <- readFile filnavn
                                        let cmd = tokenize filInnhold
                                        if (all isDigit(head cmd))
                                            then 
                                                let n = (read (head cmd)) in
                                                if ((0 < n) && (n < 100)) 
                                                then do
                                                c (read (head cmd))
                                                parseFil n (tail cmd) 
                                                else putStrLn (show n ++ " er utenfor grid")
                                            else putStrLn ("Filen " ++ filnavn ++ " inneholder uloverlig input")
                                        main
                                False -> do putStrLn ("Filen " ++ filnavn ++ " eksisterer ikke")
                                            main
        _ -> do  -- unknown command
            putStrLn "Må skrive c <x> eller r <filename>" 
            main

gameLoop :: Int -> Board -> [Int] -> [Int] -> IO()
gameLoop str alive b s = do 
    clrInput
    goto(0, str +5)
    command <- getLine -- Get commando
    let cs = words command
    case cs of 
        ("n":xs) -> do
                    printError "" str
                    if((checkN str xs))
                    then do
                            n xs str
                            let newCells = alive ++ tuples xs 
                            gameLoop str newCells b s
                    else do
                        n xs str
                        gameLoop str alive b s
                    
        ("e":xs) -> do
                    printError "" str
                    if((checkN str xs))
                    then do
                            e xs str
                            let newCells = removeCells (tuples xs) alive
                            gameLoop str newCells b s
                    else do 
                        e xs str
                        gameLoop str alive b s

        ("b":x:y:[]) -> do 
                    printError "" str
                    if(allNumbers [x,y] && inGrid x y) 
                    then do
                        goto(0, str +5)
                        clrInput
                        gameLoop str alive [read x .. read y] s
                    else do
                        printError "Ugyldig input: maks antall mulige naboer er 8" str
                        gameLoop str alive b s
        ("s":x:y:[]) -> do
                    printError "" str
                    if(allNumbers [x,y] && inGrid x y)  
                    then do
                        goto(0, str +5)
                        clrInput
                        gameLoop str alive b [read x .. read y] 
                    else do
                        printError "Ugyldig input: maks antall mulige naboer er 8" str
                        gameLoop str alive b s
        ("w":[]) -> do 
                    w str alive
                    gameLoop str alive b s
        ("?":[]) -> do
                    regler str b s
                    gameLoop str alive b s
        ([]) -> do 
                printError "" str
                enter alive b s str
                --gameLoop str newCells emptyRule aliveRule
        ("l":x:[]) -> do 
                    printError "" str
                    if(allNumbers[x])
                    then livemode (read x) alive b s str
                    else gameLoop str alive b s--feilmelding må med
        ("quit":[]) -> return () -- quit 
        _ ->    do 
                printError "Må skrive en kommando" str
                gameLoop str alive b s

removeCells :: Eq a => [a] -> [a] -> [a]
removeCells (x:[]) b = if elem x b then filter (/= x) b else b
removeCells (x:xs) b    | elem x b = removeCells xs (filter (/= x) b)
                        | otherwise = removeCells xs b


allNumbers :: Foldable t => [t Char] -> Bool
allNumbers xs | and (map (all isDigit) xs) = True
              | otherwise = False

c :: Int -> IO()
c n | n <= 0 = putStrLn "Må være større enn 0" 
    | n >= 100 = putStrLn "Må være mindre enn 100"
    | otherwise = do    clr
                        writeTop n
                        writeRows n 

n :: [String] -> Int -> IO ()
n xs str 
        | mod ((length xs)+1) 2 == 0 = printError "Ikke partall antall nummer" str
        | not (checkN str xs) = printError "Utenfor grid eller ikke tall" str
        | otherwise = writeCells(tuples xs) str "O"
                         

e :: [String] -> Int -> IO()
e xs str 
        | mod ((length xs)+1) 2 == 0 = printError "ikke partall antall nummer" str
        | not (checkN str xs) = printError "Utenfor grid eller ikke tall" str
        | otherwise = writeCells(tuples xs) str "."

w :: Int -> Board -> IO()
w n b = do 
            goto(0, n+2)
            putStr "\ESC[0J"
            putStr "Alle levende celler på brettet: " 
            putStr $ show $ b

regler :: Int -> [Int] -> [Int] -> IO()
regler str b s = do 
    goto(0, str+3)
    putStr "\ESC[0J"
    putStrLn("En tom celle med minimum " ++ show (head b) ++" og maks " ++ show (last b) ++ " naboer vil komme til livet")
    putStrLn("En levendre celle med minimum " ++ show (head s) ++" og maks " ++ show (last s) ++ " naboer vil overleve") 

livemode :: Int -> Board -> [Int] -> [Int] -> Int -> IO()
livemode times cells b s str    | times > 0 = do
                                    let newCells = (step cells b s str)
                                    if (newCells == cells) 
                                        then do
                                        printError "You have reach a stable configuration" str
                                        gameLoop str cells b s
                                    else do   
                                        writeCells cells str "."
                                        writeCells newCells str "O"
                                        wait 500000
                                        livemode (times-1) newCells b s str
                                | otherwise = do
                                                printError "The configuration is complete" str
                                                gameLoop str cells b s
wait :: Int -> IO()
wait n = sequence_[return() | _ <- [1..n]]

generer :: Board -> Int -> [Int] -> [Int] -> Int -> String -> IO ()
generer cells str b s = writeCells(step cells b s str)

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not(isAlive b p)

survivors :: Int -> Board -> [Int] -> Board 
survivors str cells s = [p | p <- cells, elem (levendeNaboer str cells p) s]

births :: Int -> Board -> [Int] -> Board
births str cells b = [p | p <- rmdups (concat (map (naboer (str)) cells)),
                    fst  p <= str, 
                    snd p <= str, 
                    isEmpty cells p, 
                    elem (levendeNaboer str cells p) b] 

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)


step :: Board -> [Int] -> [Int] -> Int -> Board
step cells b s str = survivors str cells s ++ births str cells b


enter :: Board -> [Int] -> [Int] -> Int -> IO ()
enter cells b s str = do
            let newCells = (step cells b s str)
            if (newCells == cells) 
                then do
                printError "You have reach a stable configuration" str
                gameLoop str cells b s
            else do   
                writeCells cells str "."
                writeCells newCells str "O"
                gameLoop str newCells b s
        

levendeNaboer :: Int -> Board -> Pos -> Int
levendeNaboer str cells = length . filter (isAlive cells) . (naboer (str))

naboer:: Int -> Pos -> Board
naboer str (x,y) =  if(y==1 && x==1) then 
                        [(x,y+1), (x+1, y+1), (x+1, y)]
                    else if(y==1 && x==str) then
                        [(x-1,y), (x-1,y+1), (x,y+1)]
                    else if(y==str && x==1) then
                        [(x, y-1), (x+1, y-1), (x+1, y)]  
                    else if(y==str && x==str) then
                        [(x-1,y-1), (x-1,y), (x, y-1)] 
                    else if (x==1) then 
                        [(x,y+1), (x, y-1), (x+1, y+1), (x+1, y-1), (x+1, y)] 
                    else if(y==1) then 
                        [(x-1,y), (x-1,y+1), (x,y+1), (x+1, y+1),(x+1, y)]
                    else if(y==str) then  
                        [(x-1,y-1), (x-1,y), (x, y-1), (x+1, y-1), (x+1, y)]  
                    else if(x==str) then
                        [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y+1), (x, y-1)]
                    else if (x>0 && x<str && y>0 && y<str) then
                        [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y+1), (x, y-1), (x+1, y+1),(x+1, y-1), (x+1, y)]
                    else []
    
--Sjekke at ingen av coordinatene er < 0 eller > maxIdx
{-
(x-1, y-1)  (x, y-1) (x+1, y-1)
(x-1, y)    (x, y)   (x+1, y)
(x-1, y+1)  (x, y+1) (x+1, y+1)
-}




checkN :: (Ord t, Num t, Read t) => t -> [[Char]] -> Bool
checkN _ [] = True
checkN _ (_:[]) = False
checkN n (x:y:xs)   
        | and (map (all isDigit) [x,y]) = let (nx,ny) = (read x, read y) in -- Check that coordinates are numbers
                                            if (nx > n || nx < 1 || ny > n || ny < 1) -- Check bounds 
                                            then False
                                            else checkN n xs
        | otherwise = False



writeCells :: Board -> Int -> String -> IO()
writeCells b n tegn = do
                --goto (0,0) 
                sequence_ [write p tegn | p <- b]
                goto (0,n+3)
                

tuples :: [String] -> Board
tuples [] = []
tuples (x:y:[]) = [((read x :: Int), (read y :: Int))]
tuples (x:y:xs) = [((read x :: Int), (read y :: Int))] ++ tuples xs


writeTop :: Int -> IO ()
writeTop n = do
    goto (0,0)
    putStr $ spaces 2
    mapM_ (putStr . (\nb -> spaces (3 - l nb) ++ show nb)) [1..n]

writeRows :: Int -> IO ()
writeRows n = do
    putStrLn ""
    mapM_ (putStr . (brettRow n)) [1..n]

brettRow :: Int -> Int -> String
brettRow cols row = spaces (2 - l row) ++ (show row)
                    ++ concat ["  ." | _  <- [1..cols]] ++ "\n"
                   
printError :: String -> Int -> IO() --Funksjon for å skrive ut på "error linjen"
printError s n = do
    goto (0,(n+3)) -- Move cursor
    clrInput
    putStrLn s 

write :: Pos -> String -> IO()
write (x,y) xs = do putStr("\ESC[" ++ show (y+1) ++ ";" ++ show (antP x) ++ "H")
                    putStr xs

antP :: Int -> Int
antP n = (2+(n*3))

clr :: IO()
clr = putStr "\ESC[2J"

clrInput :: IO()
clrInput = putStr "\ESC[0J"

ve :: Int
ve = 3

spaces :: Int -> [Char]
spaces n = replicate n ' '

l :: Int -> Int
l = length . show

goto :: Pos -> IO()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('b':xs) = "b": tokenize xs
tokenize ('s':xs) = "s": tokenize xs
tokenize ('(':xs) = "(": tokenize xs
tokenize (')':xs) = ")": tokenize xs
tokenize (',':xs) = ",": tokenize xs
tokenize (x:xs) 
    | isDigit x = let (tall, r) = span isDigit (x:xs) in
        tall : tokenize r
    | isAlpha x = let (ord, r) = span isAlpha (x:xs) in
        ord : tokenize r
    | otherwise = error("Invalid syntax at " ++ [x]) 

-- 5 (s 2 3, b 3 3) 3 3 1 3 2 1 3 2 2 3




parseFil :: Int -> [[Char]] -> IO ()
parseFil str ("(":"s":s1:s2:",":"b":b1:b2:")":xs) = if (parseTall b1 b2 str && parseTall s1 s2 str && parseN str xs) 
                                        then do
                                            n (xs) str
                                            gameLoop str (tuples xs) [read b1..read b2] [read s1..read s2]
                                        else do 
                                            clr
                                            printError "Ugyldig input i fil" str
parseFil str ("(":"b":b1:b2:",":"s":s1:s2:")":xs) = if (parseTall b1 b2 str && parseTall s1 s2 str && parseN str xs) 
                                        then do
                                            n (xs) str
                                            gameLoop str (tuples xs) [read b1..read b2] [read s1..read s2]
                                        else do 
                                            clr
                                            printError "Ugyldig input i fil" str

parseTall :: (Ord a, Read a) => [Char] -> [Char] -> a -> Bool
parseTall x y n
    | (all isDigit x && all isDigit y) && inGrid x y = True
    | otherwise = False

inGrid :: String -> String -> Bool
inGrid x y = if read x < 9 && read x >= 0 && read y < 9 && read y >= 0 then True else False



parseN :: (Ord t, Num t, Read t) => t -> [[Char]] -> Bool
parseN str (xs) | mod ((length xs)+1) 2 == 0 = False
                | not (checkN str xs) = False
                | otherwise = True

