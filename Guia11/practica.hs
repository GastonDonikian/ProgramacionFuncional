-- Practica Nro 11

data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Aceitunas Int
    | Jamon
    | Queso
    | Salsa

-- f. aux
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

tieneLactosa :: Ingrediente -> Bool
tieneLactosa Queso = True
tieneLactosa _     = False
-- ejercicio 1

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p) = boolToInt (f i) + cantidadCapasQueCumplen f p

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p )

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza = Prepizza
soloLasCapasQue f (Capa i p) = if f i then Capa i (soloLasCapasQue f p) else soloLasCapasQue f p

-- ejercicio 2
sinLactosa :: Pizza -> Pizza
aptaIntoleranteLactosa :: Pizza -> Bool
cantidadDeQueso :: Pizza -> Int
conElDobleDeAceitunas :: Pizza -> Pizza

-- ejercicio 3
-- pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
foldPizza :: (Ingrediente -> b -> b) -> b -> Pizza -> b
foldPizza fr cb Prepizza   = cb
foldPizza fr cb (Capa i p) = fr i (foldPizza fr cb p)

-- ejercicio 4
cantidadCapasQueCumplen' f = foldPizza (\i r -> boolToInt (f i) + r) 0
conCapasTransformadas' f  = foldPizza (Capa . f) Prepizza
soloLasCapasQue' f = foldPizza (\i r -> if f i then Capa i r else r) Prepizza

sinLactosa = foldPizza (\i r -> if tieneLactosa i then r else Capa i r) Prepizza
aptaIntoleranteLactosa = foldPizza (\i r -> not (tieneLactosa i) && r) True


primerasNCapas' :: Int -> Pizza -> Pizza
primerasNCapas' n Prepizza = Prepizza
primerasNCapas' n (Capa i p) = if n == 0 then Prepizza else Capa i (r (n-1))

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas n p = foldPizza (\i r n -> if n == 0 then Prepizza else Capa i (r (n-1))) (const Prepizza) p n

-- Ejercicio 7 
-- g. 
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f x []     =  [x]
scanr f x (y:ys) =  f y x : scanr f (f y x) ys
-- Ejercicio 9

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b [] = b
foldr' f b (x:xs) = f x (foldr' f b xs)


sum :: [Int] -> Int
sum = foldr (+) 0

length :: a -> Int
length = foldr (\_ n -> 1 + n) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x r -> f x:r) []

filter :: (a -> Bool) -> [a] -> [b]
filter f = foldr (\x r -> if f x then x:r else r) []
find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\x r -> if f x then Just x else r) Nothing

any :: (a -> Bool) -> [a] -> Bool
any f = foldr ((||) . f) False
all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\x r -> f x && r) True
countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\x r -> if f x then 1 + r else r) 0
partition :: (a -> Bool) -> [a] -> ([a],[a])
partition f = foldr
    (\x r -> if f x
        then (x : fst r, snd r)
        else (fst r, x : snd r)
        )
    ([],[])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr (\ x r ys -> if null ys then [] else f x (head ys):r (tail ys)) (const [])

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\x r -> if f x then x:r else []) []

take :: Int -> [a] -> [a]
take n xs = foldr (\ x r n -> if n == 0 then [] else x : r (n-1)) (const []) xs n

drop :: Int -> [a] -> [a]
drop = flip (foldr (\ x r n -> if n == 0 then x:r 0 else r (n-1)) (const []))

(!!) :: [a] -> Int -> a
(!!) = foldr (\x r n -> if n == 0 then x else r (n-1)) (error "...")

