import System.Exit

digitToString digit =
    case digit of 
        0 -> print "Zero"
        1 -> print "One"
        2 -> print "Two"
        3 -> print "Three"
        4 -> print "Four"
        5 -> print "Five"
        6 -> print "Six"
        7 -> print "Seven"
        8 -> print "Eight"
        9 -> print "Nine"
        _ -> print "Please only enter single digit positive numbers"

sumOfDigits n =
    if n < 10
        then n
    else (mod n 10) + (sumOfDigits $ floor (fromIntegral n / 10))
        
readUntil = do
    line <- getLine
    if line == "end"
        then do
            exitWith ExitSuccess
    else do
        print (sumOfDigits (read line :: Integer))
        readUntil

main = do
    readUntil
