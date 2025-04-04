-- a. cuadruple x = doble (doble x)
cuadruple = compose doble doble
-- b. timesTwoPlusThree x = suma (doble x) 3
timesTwoPlusThree = flip (compose suma doble) 3
-- c. fourTimes f x = f (f (f (f x)))
fourTimes = twice twice 
