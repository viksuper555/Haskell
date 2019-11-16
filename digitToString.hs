digitToString n =
    case n of 
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


main = do
    inp <- getLine
            if (inp == "End") 
                then putStrLn "Bye!"
            else digitToString inp
        main
