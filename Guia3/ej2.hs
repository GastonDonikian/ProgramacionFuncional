-- a.
apply :: (a -> a) -> a -> a
apply f x = f x
-- b.
twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- c.
id :: a -> a
id x = x
-- d.
flip :: (b -> a -> c) -> a -> b -> c
flip f x y = f y x
-- e.
uflip :: ((a,b) -> c) -> (b,a) -> c
uflip f p = f (swap p)
swap (x,y) = (y,x)
-- f.
const :: a -> b -> a
const x y = x
-- g.
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
