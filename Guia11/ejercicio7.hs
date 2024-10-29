map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x: map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f []     = []
filter' f (x:xs) = if f x then x:filter' f xs else filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b []     = b
foldr' f b (x:xs) = f x (foldr' f b xs)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr cb g [] = cb
recr cb g (x:xs) = g x xs (recr cb g xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' f [] (y:ys)         = error "different size"
zipwith' f (x:xs) []     = error "different size"
zipwith' f [] []         = []
zipwith' f (x:xs) (y:ys) = f x y:zipwith' f xs ys

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f x [] = [x]
scanr' f x (y:ys) = f y x: scanr' f (f y x) ys



-- con foldr

sum :: [Int] -> Int
sum = foldr (+) 0

-- 1:2:3:4:5:[]
-- 1+2+3+4+5+0

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr (\i r -> f i:r) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\i r -> if f i then i:r else r) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\i r -> if f i then (Just i) else r) Nothing

any :: (a -> Bool) -> [a] -> Bool
any f = foldr ((||).f) False

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr ((&&).f) True

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\i r -> if f i then r + 1 else r) 0

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition f = foldr (\i r -> if f i
                      then (i:fst r, snd r)
                      else (fst r, i: snd r)
                    )
                    ([],[])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr (\i r ys -> case ys of
                      [] -> error "..."
                      (y:ys') -> f i y:r ys') (\ys -> case ys of
                                                  [] -> []
                                                  (y:ys') -> error "...")

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f acc = foldr (\i r -> f i (head r): r) [acc]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\i r -> if f i then i:r else r) []

take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs) = if n == 0 then [] else x: take' (n-1) xs

take'' :: Int -> [a] -> [a]
take'' n xs = f xs n
  where
    f [] _ = []
    f (x:xs) n = if n == 0 then [] else x : f xs (n-1)

take''' :: Int -> [a] -> [a]
take''' n xs = f xs n
  where
    f [] = \_ -> []
    f (x:xs) = \n -> if n == 0 then [] else x: f xs (n-1)

take :: Int -> [a] -> [a]
take n xs = foldr (\x r n -> if n == 0 then [] else x : r (n-1)) (\_ -> []) xs n


drop' n xs = f xs n
  where
    f [] _ = []
    f (x:xs) n = if n == 0 then x:drop' 0 xs else drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs =
  foldr (\x r n -> if n == 0 then x:r 0 else r (n-1)) (\_ -> []) xs n

(!!) :: [a] -> Int -> a
(!!) = foldr (\x r n -> if n == 0 then x else r (n-1)) (\_ -> error "...")
