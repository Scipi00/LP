myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myIterate :: (a -> a) -> a -> [a]
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myMap :: (a -> b) -> [a] -> [b]
myFilter :: (a -> Bool) -> [a] -> [a]
myAll :: (a -> Bool) -> [a] -> Bool
myAny :: (a -> Bool) -> [a] -> Bool
myZip :: [a] -> [b] -> [(a, b)]
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
