data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT a t1 t2) = a + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT a t1 t2) = 1 + sizeT t1 + sizeT t2

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmptyT = False
anyT f (NodeT a t1 t2) = f a || anyT f t1 || anyT f t2

countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT = 0
countT f (NodeT a t1 t2) = boolToInt (f a) + countT f t1 + countT f t2

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

countLeaves :: Tree a -> Int
countLeaves EmptyT = 1
countLeaves (NodeT a t1 t2) = countLeaves t1 + countLeaves t2

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT a t1 t2) = 1 + max (heightT t1) (heightT t2)

inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT a t1 t2) = inOrder t1 ++ [a] ++ inOrder t2

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT a t1 t2) = [a]:joinLists (listPerLevel t1) (listPerLevel t2)


joinLists :: [[a]] -> [[a]] -> [[a]]
joinLists [] y = y
joinLists x [] = x
joinLists (x:xs) (y:ys) = (x ++ y):joinLists xs ys

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT a t1 t2) = NodeT a (mirrorT t2) (mirrorT t1)

levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n t = case t of
               EmptyT ->        error "Not enough depth"
               (NodeT x t1 t2) -> levelN (n-1) t1 ++ levelN (n-1) t2

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = x:pickLongest (ramaMasLarga t1) (ramaMasLarga t2)

pickLongest :: [a] -> [a] -> [a]
pickLongest xs ys = if len xs > len ys then xs else ys

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) =
  appendToAll x (todosLosCaminos t1 ++ todosLosCaminos t2)

appendToAll :: a -> [[a]] -> [[a]]
appendToAll x [] = [[x]]
appendToAll x (xs:xss) = (x:xs):appendToAll x xss
