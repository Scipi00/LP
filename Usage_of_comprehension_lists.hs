myMap :: (a -> b) -> [a] -> [b] --that emulates map using comprehension lists.
myMap f l = [f(x) | x <- l]
{-
myFilter :: (a -> Bool) -> [a] -> [a] --that emulates filter using comprehension lists.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] --that emulates zipWith using comprehension lists and zip.
thingify :: [Int] -> [Int] -> [(Int, Int)] --that, given two lists of integers, returns the list that pairs the elements if the element of the second list divides the one in the first list.
factors :: Int -> [Int] --that, given a non-null natural number, generates the ordered list with all its factors (non necessaryly primes).
-}
