twice :: (a -> a) -> a -> a
twice f x = f (f x)

uflip :: ((a,b) -> c) -> (b,a) -> c
uflip f p = f (swap p)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

const x = g
    where g y = x

appDup f = g 
    where g x = f (x, x)

appFork (f, g) = h
    where h x = (f x, g x)

appPar (f, g) = h
    where h (x, y) = (f x, g y)

appDist f = g
    where g (x, y) = (f x, f y)

flip f = h
    where h x = k
                where k y = (f y) x

subst f = h
    where h g = k
                where k x = (f x) (g x)
