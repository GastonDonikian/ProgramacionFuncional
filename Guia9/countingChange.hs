-- Version adaptada de Counting Change SICP Pag 41

-- Dado un numero arbitrario (amount) cuantas formas hay de
-- Armar cambio para ese numero dado monedas de [0.5, 0.25, 0.1, 0.05, 0.01]
-- Ignorando el orden de las monedas, de tal manera que
-- [0.5, 0.1] y [0.1, 0.5] solo sea contado una vez para armar 0.6
-- Se puede considerar infintas monedas de cada tipo

data Coin = HalfDollar | Quarter | Dime | Nickel | Penny
-- Definamos countChange que toma un amount
-- una lista de monedas disponibles
-- y devuelve la cantidad de formas de contar esas monedas

countChange :: Int -> [Coin] -> Int
countChange 0 cs = 1
countChange amount [] = 0
countChange amount (c:cs) = if amount < 0 then 0
                            else
                              countChange amount cs
                              +
                              countChange (substractCoin amount coin) (c:cs)
-- La solucion de arriba se basa en lo siguiente:
-- El numero de formas de cambiar un amount a con n tipos de moneda
-- Es lo mismo a:
  -- El numero de formas de cambiar a con cs (Todos menos el mayor)
  -- +
  -- El numero de formas de cambiar (a - mayor_denom_de_n) con (c:cs)

-- Reduciendo asi recursivamente el problema nos queda esa solucion.
-- Notese que el orden en el que contamos las monedas va a ser ordenado
  -- De mayor a menor. 


substractCoin :: Int -> Coin -> Int
substractCoin x HalfDollar = x - 0.5
substractCoin x Quarter = x - 0.25
substractCoin x Dime = x - 0.1
substractCoin x Nickel = x - 0.05
substractCoin x Penny = x - 0.01
