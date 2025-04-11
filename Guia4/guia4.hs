-- Ejercicio 1.

-- a. Funcion parcial, div x 0 da bottom
udiv (x, y) = div x y
-- b. Funcion parcial, udivE (x, 0) da bottom
udivE (x, 0) = error "No puedo dividir por 0"
udivE (x, y) = div x y
-- c. Funcion parcial, udivH (x,0) da bottom
udivH = uncurry div
-- d. Funcion total
succ x = x + 1
-- e. Funcion total
succH = suma 1
-- f. Funcion total
porLaMitad = flip div 2
-- g. Funcion parcial, conDieresis 'L' rompe
conDieresis 'u' = 'U'
-- h. Funcion parcial, nunca termina en 'L'
conDieresisB 'u' = 'U'
conDieresisB c = conDieresisB c
-- i. Funcion parcial, faltan casos
conTildePM 'a' = 'a'
conTildePM 'e' = 'e'
conTildePM 'i' = 'i'
conTildePM 'o' = 'o'
conTildePM 'u' = 'u'
-- j. Funcion parcial, bottom
conTildeE c = if esVocal c
                 then conTildePM c
              else error "El valor recibido no es vocal"
-- k. Funcion total
conTilde c = if esVocal c && esMinuscula c
                then conTildePM c
             else c

-- Ejercicio 2
-- udiv y udivH
-- succ y succH
-- conDieresis y conDieresisB
-- conTildePM y conTildeE

-- Ejercicio 3.
twice = \f -> \x -> f (f x)
-- a. 2 veces
-- twice doble
-- (\f -> \x -> f (f x)) doble
-- (\x -> doble (doble x)

-- b. 8 veces
-- twice doble 2
-- (\f -> \x -> f (f x)) doble 2
-- (\x -> doble (doble x)) 2
-- doble (doble 2)
-- doble (2 * 2)
-- doble 4
-- 4 * 2
-- 8

-- c. 1 vez
-- twice
-- (\f -> \x -> f (f x))

-- Ejercicio 4.
-- Ejercicio 5.

-- Ejercicio 6.
-- a. No hay
-- b. id int = error "..."
-- c. f x = error "..." 

