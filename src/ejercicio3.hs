-- Ejercicio 3
-- Autores:
--  - Denylson Romero
--  - Daniel Marin

-- a)
primerEjercicio :: [Int]
primerEjercicio = take 5 [x | x <- [4,2..]]

-- b)
potencias :: Int -> [Int]
potencias n = [2^x | x <- [0..n-1]]

-- c)
impares :: Int -> [Int]
impares n = take n [x | x <- [1,3..]]

-- d)
replicar :: Int -> a -> [a]
replicar n x = [y | y <- map (const x) [1..n]]

-- e)
caracteres :: Int -> Maybe String
caracteres x
  | x >= 0 && x < 27 = Just ([y | y <- ['A'..], (fromEnum y) < (fromEnum 'A' + x)])
  | otherwise = Nothing

-- f)
normasMenoresQue :: Float -> [(Int, Int)]
normasMenoresQue x = [(a,b) | a <- [-y..y], b <- [-y..y], sqrt(fromIntegral((a^2 + b^2))) <= x] where y = round x

-- g)
factores :: Int -> [Int]; factores 1 = []
factores n 
  | factor == [] = [n] 
  | otherwise = factor ++ factores (n `div` (head factor)) 
    where factor = take 1 [y | y <- [2..n-1],  n `mod` y == 0]
