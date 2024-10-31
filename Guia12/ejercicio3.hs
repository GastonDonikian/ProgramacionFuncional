boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT ct f EmptyT = ct
foldT ct f (NodeT x t1 t2) = f x (foldT ct f t1) (foldT ct f t2)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (\x r1 r2 -> NodeT (f x) r1 r2)

sumT :: Tree Int -> Int
sumT = foldT 0 (\x r1 r2 -> x + r1 + r2)

sizeT :: Tree a -> Int
sizeT = foldT 0 (\x r1 r2 -> 1 + r1 + r2)

heightT :: Tree a -> Int
heightT = foldT 0 (\x r1 r2 -> 1 + max r1 r2)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\x r1 r2 -> [x] ++ r1 ++ r2)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\x r1 r2 -> r1 ++ [x] ++ r2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\x r1 r2 -> r1 ++ r2 ++ [x])

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (\x r1 r2-> NodeT x r2 r1)

countBy :: (a -> Bool) -> Tree a -> Int
countBy f = foldT 0 (\x r1 r2 -> boolToInt (f x) + r1 + r2)
