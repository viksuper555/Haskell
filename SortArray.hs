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

main = do
    print $ mergeSort [1, 5, 6, 2, 3, 10, 8, 4]