insert :: [Int] -> Int -> [Int]
insert [] x = x:[]
insert (y:xs) x
    | x <= y = x:y:xs
    | otherwise = y:(insert xs x)
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x
remove :: [Int] -> Int -> [Int]
remove [] x = []
remove (y:s) x
    | x == y = s
    | otherwise = [y] ++ (remove s x)

myMin :: [Int] -> Int
myMin [x] = x
myMin (x:xs) = min x (myMin xs)
ssort :: [Int] -> [Int]
ssort [] = []
--ssort [x] = [x]
ssort l = [selection] ++ ssort (remove l selection)
    where
        selection = myMin l

myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys)
    | x < y = [x] ++ (merge xs (y:ys))
    | otherwise = [y] ++ (merge (x:xs) ys)
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge (msort (take half_l l)) (msort (drop half_l l))
    where
        half_l = div n 2
        n = myLength l

qsort :: [Int] -> [Int]
qsort [] = []
qsort [a] = [a]
qsort (l:ls) = (qsort minor) ++ equal ++ (qsort major)
    where
        pivot = div (l + last ls) 2
        minor = [x | x <- (l:ls), x < pivot]
        major = [x | x <- (l:ls), x > pivot]
        equal = [x | x <- (l:ls), x == pivot]
        
genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (pivot:xs) = (genQsort minor) ++ [pivot] ++ (genQsort major)
    where
        minor = [x | x <- xs, x < pivot]
        major = [x | x <- xs, x >= pivot]
        
