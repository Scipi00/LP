data Queue a = Queue [a] [a]
     deriving (Show)
 
create :: Queue a
create = Queue [] []
push :: a -> Queue a -> Queue a
push i (Queue xs ys) = Queue xs (i:ys)
pop :: Queue a -> Queue a
pop (Queue (x:xs) ys ) = (Queue xs ys)
pop (Queue [] ys ) = pop (Queue (reverse ys) [])
top :: Queue a -> a
top (Queue (x:xs) _ ) = x
top (Queue [] [y] ) = y
top (Queue [] (y:ys) ) = top (Queue [] ys )
empty :: Queue a -> Bool
empty (Queue [] []) = True
empty otherwise = False

instance Eq a => Eq (Queue a)
    where
        (Queue xs ys) == (Queue xt yt) = (xs ++ reverse ys) == (xt ++ reverse yt)
