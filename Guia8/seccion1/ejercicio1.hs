length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) = (y == x) || elem' y xs

all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs

count' :: (a -> Bool) -> [a] -> Int
count' f [] = 0
count' f (x:xs) = boolToInt (f x) + count' f xs

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

subset :: Eq a => [a] -> [a] -> Bool
subset [] ys = True
subset (x:xs) ys = elem' x ys && subset xs ys

(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x:xs) ys = x:(xs+++ys)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs +++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] ys = error "Can't zip different length lists"
zip' xs [] = error "Can't zip different length lists"
zip' (x:xs) ys = (x,head ys):zip' xs (tail ys)

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' (x:xs) = let rec = unzip' xs in
                  (fst x:fst rec, snd x: snd rec)
