data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving (Show)
size :: Tree a -> Int
size Empty = 0
size (Node x y z) = 1 + size(y) + size(z)
height :: Tree a -> Int
height Empty = 0
height (Node x y z) = 1 + max (height y) (height z)
equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node x y z) Empty = False
equal Empty (Node x y z) = False
equal (Node x1 y1 z1) (Node x2 y2 z2) = (x1==x2) && (equal y1 y2) && (equal z1 z2)
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic (Node x y z) Empty = False
isomorphic Empty (Node x y z) = False
isomorphic (Node x1 y1 z1) (Node x2 y2 z2) = (x1==x2) && (((isomorphic y1 y2) && (isomorphic z1 z2)) || ((isomorphic y1 z2) && (isomorphic z1 y2)))
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x y z) = [x] ++ preOrder y ++ preOrder z
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x y z) = postOrder y ++ postOrder z ++ [x]
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x y z) = inOrder y ++ [x] ++ inOrder z
breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst (Node t p q) = breadthList [(Node t p q)]
    where breadthList :: [Tree a] -> [a]
          breadthList [] = []
          breadthList ((Node x Empty Empty):ns) = [x] ++ breadthList (ns)
          breadthList ((Node x Empty z):ns) = [x] ++ ( breadthList (ns++[z]) )
          breadthList ((Node x y Empty):ns) = [x] ++ ( breadthList (ns++[y]) )
          breadthList ((Node x y z):ns) = [x] ++ ( breadthList (ns++(y:[z])) )

build :: Eq a => [a] -> [a] -> Tree a
build _ [] = Empty
build _ [y] = Node y Empty Empty
build (x:xs) (y:ys) = Node x (build xs_izquierda izquierda) (build xs_derecha derecha)
    where xs_izquierda = dropWhile (\a -> notElem a izquierda) xs
          xs_derecha = dropWhile (\a -> notElem a derecha) xs
          izquierda = takeWhile (/= x) (y:ys)
          derecha = drop 1 (dropWhile (/= x) (y:ys))

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap f Empty Empty = Empty
overlap f Empty (Node x y z) = (Node x y z)
overlap f (Node x y z) Empty = (Node x y z)
overlap f (Node x1 y1 z1) (Node x2 y2 z2) = Node (f x1 x2) (overlap f y1 y2) (overlap f z1 z2)
