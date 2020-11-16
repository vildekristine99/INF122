----- Innlevering -----
--A
brett :: Int -> IO()
brett n = do  clr
              writeTop n
              writeRows 1 n 

clr :: IO()
clr = putStr "\ESC[2J"

goto :: (Int, Int) -> IO()
goto(x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeTop :: (Num a, Enum a, Show a, Ord a) => a -> IO ()
writeTop n = writeAt'(ve + 1, 0) ((concat [(show(i)) ++ if i < 10 then "  " else " " | i <- [1..n]]) ++ "\n")

ve :: Int
ve = 3

writeRows :: Int -> Int -> IO ()
writeRows i n = if i == n+1 then return()
                    else do writeRow i n 
                            writeRows (i+1) n


writeRow :: Int -> Int -> IO ()
writeRow i n =  do writeAt'(if i>9 then ve-2 else ve-1, 1+i) (show i)
                   putStrLn (concat (take n (repeat " . ")))

writeAt' :: (Int, Int) -> String -> IO()
writeAt' (x,y) xs = do goto (x,y)
                       putStr xs

n :: Int -> Int -> IO ()
n x y = do      putStr "\ESC7"
                write(x,y) "X"
                putStr "\ESC8" 

d :: Int -> Int -> IO ()
d x y = do      putStr "\ESC7"
                write(x,y) "."
                putStr "\ESC8" 

q :: IO ()
q = do  putStr "\ESC[2J"
        putStr "\ESC[0;0H"

write :: (Int, Int) -> String -> IO()
write (x,y) xs = do putStr("\ESC[" ++ show (y+1) ++ ";" ++ show (antP x) ++ "H")
                    putStr xs

antP :: Num a => a -> a
antP n = (1+ (n*3))


{-
første rad er ikke med 
11 rader og kolonner
1 + (3*11)
4 7 10 13 16 19 22 25 28 31 34 +3 mellom hver prikk
1 2 3  4  5  6  7  8  9  10 11
3 5 7  9  11 13     
-}