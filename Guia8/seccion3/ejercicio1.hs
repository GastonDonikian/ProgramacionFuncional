data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA ExpA

evalExpA :: ExpA -> Int
evalExpA (Cte x) = x
evalExpA (Suma x y) = evalExpA x + evalExpA y
evalExpA (Prod x y) = evalExpA x * evalExpA y

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Suma x y) =
  simplificarSumaExpA (simplificarExpA x) (simplificarExpA y)
simplificarExpA (Prod x y) =
  simplificarProdExpA (simplificarExpA x) (simplificarExpA y)
simplificarExpA x = x

simplificarSumaExpA :: ExpA -> ExpA -> ExpA
simplificarSumaExpA (Cte 0) (Cte 0) = Cte 0
simplificarSumaExpA x (Cte 0) = x
simplificarSumaExpA (Cte 0) y = y
simplificarSumaExpA x y = Suma x y

simplificarProdExpA :: ExpA -> ExpA -> ExpA
simplificarProdExpA (Cte 0) y = Cte 0
simplificarProdExpA x (Cte 0) = Cte 0
simplificarProdExpA (Cte 1) y = y
simplificarProdExpA x (Cte 1) = x
simplificarProdExpA x y = Prod x y

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte x) = 0
cantidadDeSumaCero (Suma x y) = cuentaSumaCero (Suma x y)
                                + cantidadDeSumaCero x
                                + cantidadDeSumaCero y

cuentaSumaCero :: ExpA -> Int
cuentaSumaCero (Suma (Cte 0) y) = 1
cuentaSumaCero (Suma x (Cte 0)) = 1
cuentaSumaCero x = 0
