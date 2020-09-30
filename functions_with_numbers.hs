absValue :: Int -> Int
absValue x
    | x < 0 = (-x)
    | otherwise = x

power :: Int -> Int -> Int
power x 0 = 1
power x 1 = x
power x y
    | even(y) = halfpower * halfpower
    | odd(y) = halfpower * halfpower * x
        where
            halfpower = power x (div y 2) 

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x
    | x < 0 = isPrime(absValue(x))
    | otherwise = and( zipWith (arePrime) (repeat (x) ) [2 .. limit])
--map is more elegant, but hasnt worked due to type errors and stuff :/
    where
        limit = floor ( sqrt ( fromIntegral (x) ) )

arePrime :: Int -> Int -> Bool
arePrime x y
    | x < y = (mod y x /= 0)
    | x > y = (mod x y /= 0)
    | otherwise = False

slowFib :: Int -> Int
slowFib 0 = 0 
slowFib 1 = 1
slowFib n = slowFib(n-1) + slowFib(n-2)
quickFib :: Int -> Int
quickFib 0 = 0
quickFib 1 = 1
quickFib n
    | even n = ( (2 * qfhn_1) + qfhn ) * qfhn
    | odd n = qfhn1 * qfhn1 + qfhn1_1 * qfhn1_1
    where
        halfn = div n 2
        halfn1 = div (n+1) 2
        qfhn = quickFib(halfn)
        qfhn_1 = quickFib(halfn-1)
        qfhn1 = quickFib(halfn1)
        qfhn1_1 = quickFib(halfn1-1)
        
