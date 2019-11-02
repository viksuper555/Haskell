isPrime n j =
    if j == n
        then True
    else if (mod n j) == 0
        then False
    else isPrime n (j + 1)

sumNPrimeNumbers n i sum =
    if n == 0
        then sum
     else if (isPrime i 2)
        then sumNPrimeNumbers (n-1) (i + 1) (sum + i)
    else sumNPrimeNumbers sum n (i+1)
        
sumFirstNPrimeNumbers n =
    sumNPrimeNumbers n 2 0

main = do
    print(sumFirstNPrimeNumbers 5) 