getAt n array =
    head $ drop n array

traverseString i str =
    if i < length str
        then do
            print $ getAt i str
            traverseString (i + 1) str
    else return()

getFirstNumberIndex str i =
    if (getAt i str) == ' '
        then i
    else getFirstNumberIndex str(i + 1)

main = do
    let str = "148 3"
    let firstNumIndex = getFirstNumberIndex str 0
    print (read (take firstNumIndex str) :: Integer)
    print (read (drop (firstNumIndex + 1) str) :: Integer)    
    -- traverseString 0 "Hello" 
    -- print $ getAt 3 [1, 2, 3, 4, 5]
    -- print $ drop 3 [1, 2, 3, 4, 5]
    -- print $ tail [1, 2, 3, 4, 5]
    -- print $ head [1, 2, 3, 4, 5]
    -- print $ take 3 [1, 2, 3, 4, 5]