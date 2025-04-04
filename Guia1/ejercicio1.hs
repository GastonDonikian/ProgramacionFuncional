suma x y = x + y
doble x = 2 * x
triple = \x->3 * x
succ x = x + 1
sumarDos x = 2 + x

max1 :: Int -> Int -> Int
min1 :: Int -> Int -> Int
max1 x y = if x > y then x else y
min1 x y = if x > y then y else x
const1 x = \y -> x
subst1 f g x = f x (g x)

newF = subst1 max1 (const1 0)

newF2 = \x-> subst1 min1 (const x)
