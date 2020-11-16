
clr1 = do putStr "\ESC[2J"
          putStr "\ESC[0;0H"

          if i<100 then concat (take n (repeat "  .")) else

              else concat (take n (repeat " . "))

brett :: Int -> IO()
brett n = do 
            putStr $ hjelper n 0

hjelper :: Int -> Int -> String
hjelper n x
                | x == 0 = "    " ++ skriv (" ") [1..n] ++ "\n" ++ hjelper n (x+1)
                | x == n = show x ++ concat(replicate (n) " . ")
                | x < 10 = show x ++ "  " ++ concat(replicate (n) " . ") ++ "\n" ++ hjelper n (x+1)
                | otherwise = show x ++ " " ++ concat(replicate (n) " . ") ++ "\n" ++ hjelper n (x+1)

skriv :: String -> [Int] -> String
skriv _ (x:[]) = show x
skriv n (x:xs) 
                | x > 9  = show x ++ concat(replicate 1 " ") ++ skriv n xs
                | otherwise = show x ++ concat(replicate 2 " ") ++ skriv n xs


coorx :: Int -> [(Int, Int)]
coorx n = zip [1..n] (xakse n 0 []) 

xakse :: Int -> Int -> [Int] -> [Int]
xakse n i xs   | i == 0 = xs ++ [4] ++ xakse n (4+3) xs
               | i < antP n = xs ++ [i] ++ xakse n (i+3) xs
               | i == antP n = xs ++ [i]

--oblig2

start :: IO()
start = do 
    putStr "\ESC[0J" --Clear precious input
    command <- getLine -- Get commando
    let cs = words command-- Split on spaces
    case cs of -- Check comando
        ("quit":[]) -> return () -- quit 
        ("c":x:[]) -> do  -- add X
                        c (read x)
                        start
        ("n":xs) -> do  -- remove X
                        n xs
                        start
        _ -> do  -- unknown command
            start

--writeAt :: Pos -> String -> IO()
--writeAt p xs = do goto p
                 -- putStr xs

n xs = tuples xs

tuples :: (Read a, Read b) => [String] -> [(a, b)]
tuples [] = []
tuples (x:y:[]) = [((read x), (read y))]
tuples (x:y:xs) = [((read x), (read y))] ++ tuples xs

c :: Int -> IO()
c n | n <= 0 = error' "Must be bigger than 0" n 
    | n >= 100 = error' "Must be smaller than 100" n
    | otherwise = do    clr
                        writeTop n
                        writeRows n 
                                                

instructions :: Int -> IO()
instructions n =  do 
    goto 0 (n+2)
    putStr "Enter a command:"

writeTop :: Int -> IO ()
writeTop n = do
    goto 0 0
    putStr $ spaces 2
    mapM_ (putStr . (\nb -> spaces (3 - l nb) ++ show nb)) [1..n]

writeRows :: Int -> IO ()
writeRows n = do
    putStrLn ""
    mapM_ (putStr . (brettRow n)) [1..n]

brettRow :: Int -> Int -> String
brettRow cols row = spaces (2 - l row) ++ (show row)
                    ++ concat ["  ." | _  <- [1..cols]] ++ "\n"
                   
error' :: String -> Int -> IO() --Funksjon for å skrive ut på "error linjen"
error' s n = do
    --goto 0 (n+3) -- Move cursor
    clrInput
    putStr s 

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

goto :: Int -> Int -> IO()
goto x y = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")




if not (checkN str xs) then do printError "Out of bound or not numbers" str
                                                   gameLoop str alive 
                    else if mod ((length xs)+1) 2 == 0 then do printError "ikke partall" str 
                                                               gameLoop str alive
                    else 


    
    
    --[(x-1, y-1)] ++ [(x, y-1)] ++ [(x+1, y-1)] ++ [(x-1, y)] ++ [(x+1, y)] ++
      --                  [(x-1, y+1)] ++ [(x, y+1)] ++ [(x+1, y+1)]




{-(x > 1 && y > 1) = [(x-1,y-1)] ++ [(x-1,y)] ++ [(x,y-1)] 
                    | y > 1 = [(x,y-1)]
                    | x > 1 = [(x-1,y)]
                    | x > 1 && x < str = [(x-1,y+1)] ++ [(x,y+1)] -}
    --eller
    
  --  if(x+1 > maxindex)then else
