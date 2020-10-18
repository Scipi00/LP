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
triangulars = 1 : zipWith (+) triangulars (drop 2 nats)

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
lookNsay = 1 : map (describe) lookNsay
    where
        (length takeWhile (== head i) (li) ) + head i
        describe i = count listify i --Super listify
        listify [] = []
        listify (x:xs) = [[iterate foldl (*10+) x] : listify xs]
        count [[]] = []
        count [x:xs] = ((length takeWhile (== head x) (x))*10 + head x) : count xs 
        adsfasdfasdfasdfasd

tartaglia :: [[Integer]]
tartaglia = [1] : map (\x -> zipWith (+) (x ++ [0]) ([0] ++ x) ) tartaglia
