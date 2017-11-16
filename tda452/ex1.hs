pounds :: Double -> Double
pounds kr = kr/12.7775

fahrenheit :: Double -> Double
fahrenheit c = 32 + c * 9 / 5

price :: Double -> Double
price v  | v <= 10   = 3.5 * v
         | v <= 20   = 5 + 3 * v
         | True      = price 20 + 2.5 * (v - 20)

average :: Double -> Double -> Double -> Double
average x y z = (x + y + z) / 3

days :: Integer -> Integer
days y | mod y 4 == 0 = 366
       | True         = 365

next :: Integer -> Integer
next n | mod n 2 == 0 = div n 2
       | True         = 3 * n + 1

steps :: Integer -> Integer
steps 1 = 1
steps n = 1 + steps (next n)
