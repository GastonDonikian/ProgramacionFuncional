data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving Show

pizzaEj:: Pizza
pizzaEj = Capa (Aceitunas 2) (Capa Anchoas Prepizza)

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa x p) = (+) 1 (cantidadDeCapas p)

cuentaAceitunas :: Ingrediente -> Int
cuentaAceitunas (Aceitunas x) = x
cuentaAceitunas _ = 0

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0 
cantidadDeAceitunas (Capa ing p) = (+) (cuentaAceitunas ing) (cantidadDeAceitunas p)

duplicaAceitunas :: Ingrediente -> Ingrediente
duplicaAceitunas (Aceitunas x) = Aceitunas (2*x)
duplicaAceitunas x = x

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) = (Capa . duplicaAceitunas) ing (duplicarAceitunas p)

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa ing p) = remLact ing (sinLactosa p)

remLact :: Ingrediente -> Pizza -> Pizza
remLact Queso p = p 
remLact x p = Capa x p

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza     = True
aptaIntolerantesLactosa (Capa ing p) = ((&&) . (not) . esLactosa) ing (aptaIntolerantesLactosa p)

esLactosa :: Ingrediente -> Bool
esLactosa Queso = True
esLactosa _ = False

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa ing p) = mejoraAceitunas ing (conDescripcionMejorada p)

mejoraAceitunas :: Ingrediente -> Pizza -> Pizza
mejoraAceitunas (Aceitunas x) (Capa (Aceitunas y) p) = Capa (Aceitunas (x + y)) p
mejoraAceitunas ing           p                      = Capa ing p
