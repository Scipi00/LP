eql :: [Int] -> [Int] -> Bool
eql l1 l2 = and(zipWith(==) l1 l2) && length(l1) == length(l2)
prod :: [Int] -> Int
prod l = foldl (*) 1 l
prodOfEvens :: [Int] -> Int
prodOfEvens l = prod $ filter even l
powersOf2 :: [Int]
powersOf2 = iterate (*2) 1
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = foldl (+) 0.0 $ zipWith (*) l1 l2
