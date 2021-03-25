{--------------------- CAP 4 ----------------------}

--------------------- TAREFA 4.1 ------------------
g = 6.67*(10**(-11))

fg :: Double -> Double -> Double -> Double
fg m1 m2 d = g * m1 * m2 / (d^2)

--------------------- TAREFA 4.2 ------------------

salario :: Double -> Double
salario s = s + (s*0.1) - (s*0.07)