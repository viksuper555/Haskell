repeatStr n k =
    if k == 0
        then ""
    else n ++ (repeatStr n (k-1))
    
superDigit n = 
    if n < 10
        then n
    else superDigit $ sumDigits n

    
sumDigits n =
    if n < 10
        then n
    else (mod n 10) + (sumDigits $ floor (fromIntegral n / 10))

getAt n array =
    head $ drop n array
    
getFirstNumberIndex str i =
    if (getAt i str) == ' '
        then i
    else getFirstNumberIndex str (i + 1)

main = do
    str <- getLine
    let strNum = show str 
    let firstNumIndex = getFirstNumberIndex strNum 0
    let n = (take firstNumIndex str)
    let k = (read (drop (firstNumIndex +1) str) :: Integer)
    let num = read(repeatStr n k) :: Integer
    print (super_digit num)
