data Dungeon a =
    Habitacion a
  | Pasaje (Maybe a) (Dungeon a)
  | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

-- ejercicio 2
-- formaEsquematica :: Dungeon a -> b
-- formaEsquematica (Habication x)         = g x
-- formaEsquematica (Pasaje mx d)          = f mx (formaEsquematica d)
-- formaEsquematica (Bifurcacion mx d1 d2) = h mx (formaEsquematica d1) (formaEsquematica d2)

-- ejercicio 3
cantidadDeBifurcaciones :: Dungeon a -> Int

cantidadDePuntosInteresantes :: Dungeon a -> Int

cantidadDePuntosVacios :: Dungeon a -> Int

cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int

esLineal :: Dungeon a -> Bool

llenoDe :: Eq a => a -> Dungeon a -> Bool

cantidadDeBifurcaciones (Habitacion x) = 0
cantidadDeBifurcaciones (Pasaje mx d)  = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion mx d1 d2) = 1
                                               + cantidadDeBifurcaciones d1
                                               + cantidadDeBifurcaciones d2

cantidadDePuntosInteresantes (Habitacion x) = 1
cantidadDePuntosInteresantes (Pasaje mx d) = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion mx d1 d2) =
  1
  + cantidadDePuntosInteresantes d1
  + cantidadDePuntosInteresantes d2

cantidadDePuntosVacios (Habitacion _) = 0
cantidadDePuntosVacios (Pasaje mx d)  =
  1 - countJust mx +
  cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion mx d1 d2) =
  1 - countJust mx +
  cantidadDePuntosVacios d1 +
  cantidadDePuntosVacios d2

countJust :: Maybe a -> Int
countJust Nothing     = 0
countJust (Just _)    = 1

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

cantidadDePuntosCon x (Habitacion y) = boolToInt (x == y)
cantidadDePuntosCon x (Pasaje Nothing d) = cantidadDePuntosCon x d
cantidadDePuntosCon x (Pasaje (Just y) d) = boolToInt (x == y)
                                          + cantidadDePuntosCon x d
cantidadDePuntosCon x (Bifurcacion Nothing d1 d2) =
  cantidadDePuntosCon x d1 +
  cantidadDePuntosCon x d2
cantidadDePuntosCon x (Bifurcacion (Just y) d1 d2) =
  boolToInt (x == y)       +
  cantidadDePuntosCon x d1 +
  cantidadDePuntosCon x d2

esLineal (Habitacion x) = True
esLineal (Pasaje mx d)  = True && esLineal d
esLineal (Bifurcacion _ _ _) = False


maybeEq :: Eq a => a -> Maybe a -> Bool
maybeEq x (Just y) = x == y
maybeEq _ _ = False
llenoDe x (Habitacion y) = x == y
llenoDe x (Pasaje mx d)  = maybeEq x mx && llenoDe x d
llenoDe x (Bifurcacion mx d1 d2) =
  maybeEq x mx &&
  llenoDe x d1 &&
  llenoDe x d2
