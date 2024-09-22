type Map k v = (k -> Maybe v)

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k m = m k 

emptyM :: Map k v
emptyM = \k -> Nothing

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v m = \z -> if k == z
                        then Just v
                        else m k 


deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k m = \z -> if k == z
                       then Nothing
                       else m k

-- Hacer ejercicio map de clase pasada

-- Repaso de Tipos Algebraicos

data Color = Rojo | Verde

data Persona = ConsP String Int
ejPersona :: Persona
ejPersona = ConsP "Pedro" 22
data MaybeP a = NothingP | JustP a
ejMaybe :: MaybeP Bool
ejMaybe = JustP True 

data EitherP a b = LeftP a | RightP b

-- data Nombre tipo = Constructor tipo | ...
-- data <- Nombre Reservado
-- Nombre <- Nombre del tipo
-- tipo <- Tipo
-- Constructor <- Constructor sus parametros
-- ... <- Otros Constructores

-- Ejercicio 1...
data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon
data Helado = Vasito Gusto
              | Cucurucho Gusto Gusto
              | Pote Gusto Gusto Gusto

chocoHelate consH = consH Chocolate


-- Ejercicio 2...

data DigBin = O | I

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1 

dbAsBool :: DigBin -> Bool
dbAsBool O = True 
dbAsBool I = False

dbOfBool :: Bool -> DigBin
dbOfBool True  = O
dbOfBool False = I

negDB :: DigBin -> DigBin
negDB O = I
negDB I = O 

-- Ejercicio 4

data Medida = Mm Float
            | Cm Float
            | Inch Float
            | Foot Float

asMm :: Medida -> Medida
asMm (Mm   x) = Mm x
asMm (Cm   x) = Mm (10 * x)
asMm (Inch x) = Mm (25.4 * x)
asMm (Foot x) = Mm (304.8 * x)
  
 -- Ejercicio 5
data Shape = Circle Float
           | Rect Float Float deriving Show
construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

-- Ejercicio 7

data Set a = S (a -> Bool)
belongs :: Set a -> a -> Bool
belongs (S f) x = f x

empty :: Set a
empty = S (\_ -> False)

singleton :: Eq a => a -> Set a
singleton x = S (\y -> x == y)

union :: Set a -> Set a -> Set a
union (S s) (S z) = S (\k -> s k || z k)


intersection :: Set a -> Set a -> Set a 
intersection (S s) (S z) = S (\k -> s k && z k)

-- Ejercicio 8

data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String

type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> (Exception -> b) -> b
tryCatch (Raise e) f h = h e
tryCatch (Ok a) f h = f a 

data Maybe a = Nothing | Just a
safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)




sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
sueldoGUIE nombre empleados =
     tryCatch (lookupE nombre empleados)
           mostrarInt
           (\e -> case e of
                   NotFound -> ventanaError msgNotEmployee
                   _ -> error msgUnexpected)
     where msgNotEmployee = "No es empleado de la empresa"
           msgUnexpected = "Error "
