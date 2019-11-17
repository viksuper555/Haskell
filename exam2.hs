nextSpaceIndex str separator i = 
    if null str
        then -1
    else if (take (length separator) str) == separator
        then i
    else nextSpaceIndex (tail str) separator (i + 1)

split str separator =
    if (null str) || ((nextSpaceIndex str separator 0) == -1)
        then [str]
    else (take (nextSpaceIndex str separator 0) str) : (split (drop ((nextSpaceIndex str separator 0) + (length separator)) str) separator)

rollLeft array times = (drop times array) ++ (take times array)

mapArray array mapper =
    if null array
        then []
    else (mapper (head array)) : (mapArray (tail array) mapper)



digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

main = do
    arrayInput <- getLine
    let array = (read arrayInput :: Integer)
    let result = (digits array)
    arrayInput2 <- getLine
    let n = (read arrayInput2 :: Int)
    print (rollLeft result (mod n (length result)))