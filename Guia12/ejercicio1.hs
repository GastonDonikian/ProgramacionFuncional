data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA ExpA

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n)      = c n
foldExpA c s p (Suma e1 e2) = s (foldExpA c s p e1) (foldExpA c s p e2)
foldExpA c s p (Prod e1 e2) = p (foldExpA c s p e1) (foldExpA c s p e2)


expA1 :: ExpA
expA2 :: ExpA
expA1 = Suma (Cte 1) (Cte 0)
expA2 = Prod (Cte 1) (Suma (Cte 1) (Cte 0))
expA3 = Suma (Cte (-2)) (Cte 1)

expAList = expA1 : expA2 : expA3 : []
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA
  (boolToInt . (==0))
  (+)
  (+)

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA
  (<0)
  (||)
  (||)
