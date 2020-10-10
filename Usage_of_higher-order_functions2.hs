flatten :: [[Int]] -> [Int]
flatten x = foldl (++) [] x
myLength :: String -> Int
myLength x = foldl (+) 0 $ map (const 1) x
myReverse :: [Int] -> [Int]
myReverse l = foldl (\x -> \y -> [y]++x) [] l
countIn :: [[Int]] -> Int -> [Int]
countIn ll a = map (foldl (\x -> \y -> x + f(y == a)) 0) ll
    where f :: Bool -> Int
          f t = if t then 1 else 0
firstWord :: String -> String
firstWord = takeWhile (/=' ') . dropWhile (==' ')
