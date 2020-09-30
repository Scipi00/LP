myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)
myMaximum :: [Int] -> Int
myMaximum (x:[]) = x
myMaximum (x:xs) = max x (myMaximum xs)

inmersionaverage :: ([Int], Int, Int) -> Float
inmersionaverage ([],total,len) = fromIntegral(total)/fromIntegral(len)
inmersionaverage (x:xs,total,len) = inmersionaverage(xs,total+x,len+1)
average :: [Int] -> Float
average [] = 0.0
average (xs) = inmersionaverage(xs,0,0)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]
buildPalindrome :: [Int] -> [Int]
buildPalindrome (xs) = myReverse(xs) ++ xs

remove :: [Int] -> [Int] -> [Int]
remove l [] = l
remove [] m = []
remove (x:xs) (y:ys)
    | x == y = remove xs (y:ys)
    | x /= y = (remove [x] ys ) ++ (remove xs (y:ys))
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (l:p) = l ++ flatten(p)
oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
    | odd x = ([x] ++ (fst rec),snd rec)
    | even x = (fst rec,[x] ++ (snd rec))
        where
            rec = oddsNevens xs

            
numerar :: [Bool] -> Int -> [Int]
numerar [] _ = []
numerar (x:xs) n
    | x = [n] ++ numerar xs (n+1)
    | otherwise = numerar xs (n+1)

primeDivisors :: Int -> [Int] --0 is not expected
primeDivisors 1 = []
primeDivisors x
    | x < 0 = primeDivisors(-x)
    | isPrime x = [x]
    | otherwise = numerar ( zipWith (&&) (map isPrime [2..div x 2]) (map not (zipWith arePrime [2..div x 2] (repeat x) ) ) ) 2
    

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True --degut a la implementacio del limit
isPrime x
    | x < 0 = isPrime(-x)
    | otherwise = and( zipWith (arePrime) (repeat (x) ) [2 .. limit])
--map is more elegant, but hasnt worked due to type errors and stuff :/
    where
        limit = ceiling ( sqrt ( fromIntegral (x) ) )

arePrime :: Int -> Int -> Bool
arePrime x y
    | x < y = ((mod y x) /= 0)
    | x > y = ((mod x y) /= 0)
    | otherwise = False
