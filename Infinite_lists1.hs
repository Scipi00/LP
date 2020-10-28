ones :: [Integer]
ones = 1 : ones

nats :: [Integer]
nats = 0 : map (+1) nats

ints :: [Integer]
ints = 0 : map (myNextInt) ints
    where
        myNextInt x 
            | x == 0 = 1
            | x > 0 = -x
            | x < 0 = -x +1

triangulars :: [Integer]
triangulars = 0 : zipWith (+) triangulars (drop 1 nats)

factorials :: [Integer]
factorials = scanl (*) 1 (drop 1 nats)

fibs :: [Integer]
fibs = fibs' 0 1
    where
        fibs' m n = m : fibs' n (m+n)

primes :: [Integer]
primes = garbell [2..]
    where
        garbell (p : xs) = p : garbell [x | x <- xs, (mod x p) /= 0]

hammings :: [Integer]
hammings = 1 : merge3  (map (*2) hammings)  (map (*3) hammings)  (map (*5) hammings)
    where
        merge3 xs ys zs = merge2 xs (merge2 ys zs)
        merge2 [] ys = ys
        merge2 xs [] = xs
        merge2 (x:xs) (y:ys)
            | x == y = [x] ++ merge2 xs ys
            | x < y = [x] ++ merge2 xs (y:ys)
            | x > y = [y] ++ merge2 (x:xs) ys

lookNsay :: [Integer]
lookNsay = 1 : map (numberfy.describe.show) lookNsay

--Auxiliary mylenght and other functions
myLength :: [Char] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)
describe :: [Char] -> [Char]
describe [] = []
describe i = integer_to_string(myLength (takeWhile (== head i) i) ) ++ [head i] ++ describe (dropWhile (== head i) i)
numberfy :: [Char] -> Integer
numberfy "" = 0
numberfy (x:xs) = (char_to_num x) * 10^(length(x:xs) -1) + numberfy xs
char_to_num :: Char -> Integer
char_to_num x
    | x == '0' = 0
    | x == '1' = 1
    | x == '2' = 2
    | x == '3' = 3
    | x == '4' = 4
    | x == '5' = 5
    | x == '6' = 6
    | x == '7' = 7
    | x == '8' = 8
    | x == '9' = 9
    | otherwise = 123456789 --failtest
integer_to_string :: Integer -> [Char]
integer_to_string 0 = []
integer_to_string x = integer_to_string (div x 10) ++ [num_to_char (mod x 10)]
num_to_char :: Integer -> Char
num_to_char x
    | x == 0 = '0'
    | x == 1 = '1'
    | x == 2 = '2'
    | x == 3 = '3'
    | x == 4 = '4'
    | x == 5 = '5'
    | x == 6 = '6'
    | x == 7 = '7'
    | x == 8 = '8'
    | x == 9 = '9'
    | otherwise = '*' --failtest
            

tartaglia :: [[Integer]]
tartaglia = [1] : map (\x -> zipWith (+) (x ++ [0]) ([0] ++ x) ) tartaglia
