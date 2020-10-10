--Auxiliary stuff
myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter g [] = []
myFilter g (x:xs) = if g x then [x] ++ myFilter g xs else myFilter g xs

{-Define a function countIf :: (Int -> Bool) -> [Int] -> Int that, given a predicate on integers and a list of integers, returns the number of elements in the list that satify the predicate.-}

countIf :: (Int -> Bool) -> [Int] -> Int
countIf g [] = 0
countIf g (x:xs) = if g x then 1 + countIf g xs else countIf g xs

--Auxiliary stuff
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = [f x] ++ myMap f xs

{-Define a function pam :: [Int] -> [Int -> Int] -> [[Int]] that, given a list of integers and a list of functions from integers to integers, returns the list consisting if applying each of the functions in the second list to the elements in the first list.-}

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam = 
--Auxiliary stuff

{-Define a function pam2 :: [Int] -> [Int -> Int] -> [[Int]] that, given a list of integers and a list of functions from integers to integers, returns the list of lists where each list if the result of applying, one after the other, the function in the second list to each element in the first list.-}



--Auxiliary stuff

{-Define a function filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int that returns a fold of all the elements that satisfy the given predicate.-}



--Auxiliary stuff

{-Define a function insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] that, given a relation between integers, a list and un element, return the list with the inserted element according to the relation.-}



--Auxiliary stuff

{-Use function insert, in order to define function insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] that orders a list according to the given relation.-}



