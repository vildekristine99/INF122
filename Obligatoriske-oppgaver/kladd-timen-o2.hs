-- Modelering av brettet og naboer

type Board' = [[Bool]] -- n*n array, b[x][y] == true betyr cellen på pos (x,y) er i live

type Pos'' = (Int,Int)
type Board'' = [(Pos, Bool)] -- n**2 array, b[i] == ((x,y), true) betyr cellen på pos (x,y) er i live

--boken
type Pos = (Int,Int)
type Board = [Pos] --(x,y) elem b betyr at cellen på pos(x,y) er i live


naboer:: Board -> Pos -> Int -> [Pos] -- Board -> (Int, Int) -> [Pos]
naboer b currentCell maxIdx = []
    let 
        x = min(maxIndex, x+1)

    --eller
    
    if(x+1 > maxindex)then else

--Sjekke at ingen av coordinatene er < 0 eller > maxIdx
{-
(x-1,y-1) (x, y-1)  (x+1,y-1)
(x-1, y)    (x,y)    (x+1, y)
(x-1,y+1)   (x, y+1) (x+1,y+1)
-}

data Cmd = 
    NewRule Int Int
    ...
    Undefined 

parse :: String -> Cmd
sjekke om den er undefined

case cmd of
    "s":ss -> gameLoop .. newSRule bRule
    "b":ss 
    "" -> do 
        oneStep
        gameLoop ...

--ikke all parsing i loopen, egen parser


--Enter -> Gå et steg frem i simuleringen
case cmd of
    "" -> --gjør et steg
    "l <x>" -> --live mode


step = --må vite reglene du har
    --Finn cellene som overlever
    --Finn cellene som blir født
    --fjern døde celler fra guiden
    --print (nye) levenede celler til fuiden 
    --returner nytt brett (med din modellering) f.eks. en liste over alle de levende cellene etter et sted

livemode .. 0 = return 

    --hvis alle stedene er gjort return

    --gjør et steg og få tilbake det nye brettet
    --sjekk om gammel brett og nytt brett er det samme
    --hvis ja: stable config -> return
    --hvis nei: vent noen sekunder, kall livemode rekursivt med (Steg-1) og nytt brett

det gir ikke mening å gi nye celler uten å ikke ha et brettet
to nivåer
main tar inn c eller r alt anna er ikke gyldig og vil være i andre nivå

x og y er motsatt







--egen kladd
parse :: String -> String
parse [] = []

parser :: String -> (String, String)
parser [] = error' "Missing expr" 0
parser input 
    | null r = (a,r)
    | elem (head r) commands = 
    where 
        commands = ["c", "n", "e", "b", "s", "?", "w", "r" "l", "quit"]
        (a,r) = parserArg input
--r :: String -> IO()

instructions :: Int -> IO()
instructions n =  do 
    goto (0,(n+2))
    putStr "Enter a command:"

start :: IO()
start = do 
    clrInput
    command <- getLine -- Get commando
    let cs = words command-- Split on spaces
    case cs of -- Check comando
        ("quit":[]) -> return () -- quit
        ("c":x:[]) -> do  
                        c (read x)
                        gameLoop (read x)
        --("r":xs) -> do  
                        --r xs
                        --gameLoop
        _ -> do  -- unknown command
            error' "Må skrive c <x> eller r <filename>" 0
            start

gameLoop :: Int -> IO()
gameLoop str = do 
    clrInput
    command <- getLine -- Get commando
    let cs = words command
    case cs of 
        --("n":xs) -> --do
                       -- n xs str
        ("quit":[]) -> return () -- quit 




