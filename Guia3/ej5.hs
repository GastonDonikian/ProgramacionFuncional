-- a.
appDup f = \x -> f (x, x)
-- b.
appFork (f, g) = \x -> (f x, g x)
-- c.
appPar (f, g) = \(x, y) -> (f x, g y)
-- d.
appDist f = \(x, y) -> (f x, f y)
-- e.
subst f = \g -> \x -> (f x) (g x)
