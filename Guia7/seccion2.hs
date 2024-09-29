type Nombre = String
data Planilla = Fin  | Registro Nombre Planilla
data Equipo = Becario Nombre
              | Investigador Nombre Equipo Equipo

-- ejercicio 2
-- formaEsquematica :: Planilla -> a
-- formaEsquematica Fin = ...
-- formaEsquematica (Registro xs p) = f xs (formaEsquematica p)

-- formaEsquematica :: Equipo -> a
-- formaEsquematica (Becario x) = g x
-- formaEsquematica (Investigador x e1 e2) = f x (formaEsquematica e1) (formaEsquematica e2)

-- ejercicio 3
largoDePlanilla :: Planilla  -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro xs p) = 1 + largoDePlanilla p

esta :: Nombre -> Planilla -> Bool
esta n Fin             = False
esta n (Registro xs p) = n == xs || esta n p

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p              = p
juntarPlanillas (Registro xs p1) p = Registro xs (juntarPlanillas p1 p)

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _)            = 0
nivelesJerarquicos (Investigador _ e1 e2) =
    1 + max (nivelesJerarquicos e1) (nivelesJerarquicos e2)

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario _)            = 1
cantidadDeIntegrantes (Investigador _ e1 e2) = 1
                                               + cantidadDeIntegrantes e1
                                               + cantidadDeIntegrantes e2

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario x) = Registro x Fin
planillaDeIntegrantes (Investigador x e1 e2) = Registro x
                                               (juntarPlanillas
                                                 (planillaDeIntegrantes e1)
                                                 (planillaDeIntegrantes e2))
