-- Hacer ej 1, 2. 3. 5.
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x, y)
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x, y) = f x y

compose f g x = f (g x)
(.) f g x = f (g x)
---
-- a
apply f = g
  where g x = f x
apply :: (a -> b) -> a -> b
apply f x = f x
apply f = f

-- b
twice f x = f (f x)
twice f = compose f f

-- c
id = \x -> x

-- d.
flip f = g
  where g x = h
        h y = (f y) x

flip f x y = f y x

-- e.
uflip f = g
  where g p = f (swap p)
uflip :: ((a,b) -> c) -> (b,a) -> c

swap (x,y) = (y,x)
swap :: (a,b) -> (b,a)
uflip f p = f (swap p)
-- f. 
const = \x -> (\y -> x)
const :: a -> b -> a
const x y = x
-- g.
compose = \f -> (\g -> (\x -> f (g x)))
compose f g x = f (g x)

-- 5. REESCRIBIR LAS SIGUIENTES DEFINICIONES UTILIZANDO
-- A. SOLO LAMBDAS
-- B. SIN LAMBDAS
-- C. DAR EL TIPO PARA TODAS LAS FUNCIONES

appDup :: ((a,a) -> c) -> a -> c 
appDup f = g
  where g x = f (x, x)
appDup = \f -> \x -> f (x, x)
appDup f x = f (x, x)

appFork :: (a -> b, a -> c) -> a -> (b, c)
appFork (f, g) = h
  where h x = (f x, g x)
appFork = \(f, g) -> \x -> (f x, g x)
appFork (f, g) x = (f x, g x)

appPar :: (a -> c, b -> d ) -> (a, b) -> (c, d)
appPar (f, g) = h
  where h (x, y) = (f x, g y)
appPar = \(f, g) -> \(x, y) -> (f x, g y)
appPar  (f, g) (x, y) = (f x, g y)

appDist :: (a -> b) -> (a, a) -> (b, b)
appDist f = g
  where g (x, y) = (f x, f y)
appDist = \f -> \(x,y) -> (f x, f y)
  
subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f = h
  where h g = k
        where k x = f x (g x)
subst = \f -> \g -> \x -> f x (g x)
subst f g x = f x (g x)




























-- EJERCICIO:
-- Dar el tipo de las siguientes funciones
flip f x y = f y x
appDup f x = f (x, x)
appPar (f, g) (x, y) = (f x, g y)
sumaPar (x, y) = x + y
maxPar (x, y) = max x y
neg = (*) (-1)
max x y = if x > y
             then x
             else y

min x y = if x< y
             then x
             else y

dup x = (x, x)                  

-- Decir si hay o no equivalencias entre estas expresiones:
? sumaPar = uncurry (+)                          ?
? maxPar = uncurry max                           ?
maxPar :: (Int, Int) -> Int
max :: Int -> Int -> Int
uncurry max :: (Int, Int) -> Int
? curry sumaPar = (+)                            ?
? curry maxPar = max                             ?
  
? fst = uncurry const                            ?

? snd = uncurry (flip const)                     ?
uncurry (flip const)
flip const :: a -> b -> b
uncurry (flip const) :: (a,b) -> b


-- Ejercicio 7:
-- Dada la siguiente definicion, indicar como podria reescribirse usando compose y id. 
many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

many 0 f x = id x
many n f x = compose f (many (n - 1) f) x 

many 0 f = id
many n f = compose f (many (n - 1) f)



















? compose (many n f) (many m f) = many (n + m) f ?


-- Hacer Ejercicio 7 y 8











  
