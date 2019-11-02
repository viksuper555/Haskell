pow n m =
    if m == 0
        then 1
    else n * (pow n (m-1))

main = do
    print(pow 2 2)