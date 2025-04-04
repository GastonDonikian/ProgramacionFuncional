-- a.
-- compose (fst snd) -- No es valida, (fst snd) no tiene tipo.
compose fst snd -- Si es valida
-- b.
(uncurry curry snd) -- No es valida, uncurry curry no tiene tipo
uncurry (curry snd) -- Si es valida
-- c.
apply id (id apply apply) -- Le saque parentesis redundantes, es valida
-- d.
compose (compose doble doble) -- Si es valida
-- e.
compose compose doble doble -- No es valida, no tiene tipo
compose (compose doble doble) -- Si es valida

