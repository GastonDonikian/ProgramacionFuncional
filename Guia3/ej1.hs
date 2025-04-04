curry' :: ((a,b) -> c) -> a -> b -> c
uncurry' :: (a -> b -> c) -> (a,b) -> c

curry' f x y = f (x, y)
uncurry' f (x,y) = f x y 

