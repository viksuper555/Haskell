import System.Exit

divide n m = floor $ ((fromIntegral n) / (fromIntegral m))

mergeArray firstPartition secondPartition =
    if (null firstPartition) && (null secondPartition)
        then []
    else if (null firstPartition)
        then secondPartition
    else if (null secondPartition)
        then firstPartition
    else if ((head firstPartition) < (head secondPartition))
        then (head firstPartition) : (mergeArray (tail firstPartition) secondPartition)
    else (head secondPartition) : (mergeArray firstPartition (tail secondPartition))
    
mergeSort array = 
    if (length array) < 2
        then array
    else mergeArray (mergeSort (take (divide (length array) 2) array)) (mergeSort (drop (divide (length array) 2) array))

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

mapArray array mapper =
    if null array
        then []
    else (mapper (head array)) : (mapArray (tail array) mapper)

reverseArray :: [Integer] -> Int -> Int -> [Integer]

reverseArray array start count = 
    (take start array) ++ (reverse (take count (drop start array))) ++ (drop (start + count) array)

sortArray :: [Integer] -> Int -> Int -> [Integer]

sortArray array start count = 
    (take start array) ++ (mergeSort (take count (drop start array))) ++ (drop (start + count) array)

rollLeft :: [Integer] -> Int -> [Integer]

rollLeft array times = (drop times array) ++ (take times array)

rollRight :: [Integer] -> Int -> [Integer]

rollRight array times = (drop ((length array) - times) array) ++ (take times array)
    
interprete :: String -> [Integer] -> [Integer]

interprete input array = do
    let splitCommand = (split input " ")
    let command = head splitCommand

    if command == "reverse"
        then return (reverseArray array (read (head (drop 2 splitCommand)) :: Int) (read (head (drop 4 splitCommand)) :: Int))
    else if command == "sort"
        then (sortArray array (read (head (drop 2 splitCommand)) :: Int) (read (head (drop 4 splitCommand)) :: Int))
    else if command == "rollLeft"
        then rollLeft array (read (head (drop 1 splitCommand)) :: Int)
    else if command == "rollRight"
        then rollRight array (read (head (drop 1 splitCommand)) :: Int)
    else array

-- readUntil :: [Integer] -> ()

readUntil array = do
    line <- getLine
    if line == "end"
        then do
            print array
            exitWith ExitSuccess
    else do
        let arr = interprete line array
        readUntil arr

main = do
    arrayInput <- getLine
    let splitArray = (split arrayInput " ")
    let mappedArray = mapArray splitArray (\x -> read x :: Integer)
    readUntil mappedArray