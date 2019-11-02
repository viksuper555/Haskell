repeatString :: String -> Integer -> String

repeatString str n =
    if n == 0
        then ""
    else str ++ (repeatString str (n-1))

main = do
    print (repeatString "Batman" 5)