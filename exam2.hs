rollLeft array times = (drop times array) ++ (take times array)

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

main = do
    arrayInput <- getLine
    let array = (read arrayInput :: Integer)
    let result = (digits array)
    arrayInput2 <- getLine
    let n = (read arrayInput2 :: Int)
    print (rollLeft result (mod n (length result)))