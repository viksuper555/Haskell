sumOfDigits n =
    if n < 10
        then n
    else (mod n 10) + (sumOfDigits $ floor (fromIntegral n / 10))

main = do
    line <- getLine
    print (sumOfDigits (read line :: Integer))
