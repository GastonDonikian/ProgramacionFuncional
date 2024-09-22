data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto

chocoHelate consH = consH Chocolate

// Ejercicio 1
// Vasito :: Gusto -> Helado
// Chocolate :: Gusto
// Cucurucho :: Gusto -> Gusto -> Helado
// Sambayon :: Gusto
// Pote :: Gusto -> Gusto -> Gusto -> Helado
// chocoHelate :: (Gusto -> b) -> b
// chocoHelate Vasito :: Helado
// chocoHelate Cucurucho :: Gusto -> Helado
// chocoHelate (Cucurucho Sambayon) :: Helado
// chocoHelate (Vasito DulceDeLeche) :: NO TIPA
// chocoHelate Pote :: Gusto -> Gusto -> Helado
// chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

data DigBin = O | I
dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool :: DigBin -> Bool
dbAsBool O = False
dbAsBool I = True

dbOfBool :: Bool -> DigBin
dbOfBool True = I
dbOfBool False = O

negDB :: DigBin -> DigBin
negDB I = O
negDB O = I










