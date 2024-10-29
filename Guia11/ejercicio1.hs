data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente =
  Aceitunas Int
  | Jamon
  | Queso
  | Salsa

foldPizza :: b -> (Ingrediente -> b -> b) -> Pizza -> b
foldPizza ep fc Prepizza = ep
foldPizza ep fc (Capa i p) = fc i (foldPizza ep fc p)

cantCapas :: Pizza -> Int
cantCapas = foldPizza 0 (\i r -> 1 + r)

aceitunas (Aceitunas n) = n
aceitunas _ = 0

duplicarAceitunas = foldPizza Prepizza (\i r -> Capa (duplicar i) r)

duplicar (Aceitunas n) = Aceitunas (2 * n)
duplicar x = x

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f = foldPizza 0 (\i r -> boolToInt (f i) + r)


boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f = foldPizza Prepizza (\i r -> Capa (f i) r)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f = foldPizza Prepizza
  (\i r -> if f i then (Capa i r) else r)

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = foldPizza 0 (\i r -> aceitunas i + r)

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = foldPizza Prepizza (\i r -> juntarAceitunas i r)

juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
juntarAceitunas i p = Capa i p

conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe = foldPizza id (\i r -> \p -> Capa i (r p))

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas n Prepizza = Prepizza
primerasNCapas n (Capa i p) = if n == 0 then Prepizza
  else (Capa i (primerasNCapas (n-1) p))

primerasNCapas' :: Int -> Pizza -> Pizza
primerasNCapas' n p = foldPizza (\n -> Prepizza) g p n
  where g i r 0 = Prepizza
        g i r n = Capa i (r (n-1))


sinLactosa = foldPizza Prepizza (\i r -> case i of
                                  Queso -> r
                                  x -> Capa x r) 

aptaIntolerantesLactosa = foldPizza True (\i r -> case i of
                                  Queso -> False
                                  x -> r)

cantidadDeQueso = foldPizza 0 (\i r -> case i of
                                  Queso -> 1 + r
                                  x -> r)

conElDobleDeAceitunas = foldPizza Prepizza (\i r -> case i of
                                  Aceitunas i -> Capa (Aceitunas (2*i)) r
                                  x -> Capa i r)