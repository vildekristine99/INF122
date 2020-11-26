data Command 
    = CreateBoard Int
    | ReadBoard String
    | AddCells [Pos]
    | RmCells [Pos]
    | LiveMode Int
    | BirthRule Pos
    | SurviveRule Pos
    | ShowRules
    | ShowLiving 
    | OneStep
    | Quit
    | Undefined String
    deriving Show

parseCommand :: [String] -> Command 
[] = OneStep
parseCommand ls = case ls of
    ("c":[n]) -> parseCreate n
    ("r":[fn]) -> ReadBoard fn 
    .
    .
    .
    _ -> Undefined $ "not a command: " ++ concat ls 


parseCreate s = if al isDigit s 
                    then let n = read n in
                        if(outofgrid)
                            then undefined "invalid size"
                            else CreateBoard n
                    else undefined "not number"