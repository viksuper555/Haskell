import ExitSuccess

readUntil = do
    line <- getLine
    if line == "end"
        then exitWith ExitSuccess
    else do
        interprete line
        readUntil

main = do
    readUntil