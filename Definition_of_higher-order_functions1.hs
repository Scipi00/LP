myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f x [] = x
myFoldr f x (y:ys) = f y (myFoldr f x ys)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil g f x = if g x then x else myUntil g f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = [f x] ++ myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter g [] = []
myFilter g (x:xs) = if g x then [x] ++ myFilter g xs else myFilter g xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll g [] = True
myAll g (x:xs) = if g x then myAll g xs else False

myAny :: (a -> Bool) -> [a] -> Bool
myAny g [] = False
myAny g (x:xs) = if g x then True else myAny g xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = [(x,y)] ++ myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys
