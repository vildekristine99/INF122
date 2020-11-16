import Data.Char

brett :: Int -> IO()
brett n 
        | n <= 0 = error "Must be bigger than 0" --check for valid n
        | n >= 100 = error "Must be smaller than 100"
        | otherwise = do
            putStr "\ESC[2J" --Clearing page
            goto 0 0 -- Moving cursor to top left
            putStr $ spaces 2 -- Space for row numbers
            mapM_ (putStr . (\nb -> spaces (3 - l nb) ++ show nb)) [1..n]-- Printing col-numbers
            putStrLn "" -- Going to the next line to start printing rows
            mapM_ (putStr . (brettRow n)) [1..n] -- Printing the rows

            -- B
            printInstructions n-- print intsructions
            xGame n -- start game loop

goto :: Int -> Int -> IO()
goto x y  = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

spaces :: Int -> String
spaces n = replicate n ' '

l :: Int -> Int
l = length . show

brettRow :: Int -> Int -> String
brettRow cols row = spaces (2 - l row) -- Spaces before one-digit numbers
                    ++ (show row) -- The row number
                    ++ concat ["  ." | _  <- [1..cols]] -- The dots with two spaces in front
                    ++ "\n" -- new line


--Oppg B
xGame :: Int -> IO() --Game loop
xGame n = do 
            goto 0 (n+4) --Input line below instructions and error messages
            putStr "\ESC[0J" --Clear precious input
            command <- getLine -- Get commando
            let cs = words command-- Split on spaces
            case cs of -- Check comando
                ("q":[]) -> return () -- quit 
                ("n":x:y:[]) -> do  -- add X
                                change x y n "X"
                                xGame n
                ("d":x:y:[]) -> do  -- remove X
                                change x y n "."
                                xGame n
                _ -> do  -- unknown command
                     printError "Unknown command" n
                     xGame n


change :: String -> String -> Int -> String -> IO() -- Put X og - in grid
change x y n str 
        | and (map (all isDigit) [x,y]) =   let (nx,ny) = (read x, read y) in -- Check that coordinates are numbers
                                            if (nx > n || nx < 1 || ny > n || ny < 1) -- Check bounds 
                                                then printError "Out of bounds" n
                                                else printInGrid str nx ny n 
        | otherwise = printError "Coordinates must be numbers" n


printInGrid :: String -> Int -> Int -> Int -> IO() -- Print X og - in grid
printInGrid s x y n = do 
        printError "" n -- Remove error
        goto (x*3 + 2) (y+1) -- Move cursor 
        putStr s -- Put X or .

printInstructions :: Int -> IO()
printInstructions n = do 
    goto 0 (n+2)  --Move cursor
    putStr "Enter a command: n <x> <y> ..." --Put instructions

printError :: String -> Int -> IO() --Funksjon for å skrive ut på "error linjen"
printError s n = do
    goto 0 (n+3) -- Move cursor
    putStr "\ESC[0J" -- Erase previous error message
    putStr s -- Put correct error message