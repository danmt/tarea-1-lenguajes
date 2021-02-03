-- Ejercicio 3
-- Autores:
--  - Denyl Romero
--  - Daniel Marin

-- a)
primerEjercicio :: [Int]
primerEjercicio = take 5 [4,2..]

-- b)
potencias :: Int -> [Int]
potencias 0 = []
potencias n = 1 : [2^x | x <- [1..n-1]]

-- c)
impares :: Int -> [Int]
impares n = take n [1,3..]

-- d)
replicar :: Int -> a -> [a]
replicar n x = [y | y <- map (const x) [1..n]]

-- e)
caracteres :: Int -> Maybe String
caracteres x
  | x > 0 && x < 27 = Just (take x ['A'..])
  | otherwise = Nothing

-- f)
normasMenoresQue :: Float -> [(Int, Int)]
normasMenoresQue x = [(a,b) | a <- [-y..y], b <- [-y..y], sqrt(fromIntegral((a^2 + b^2))) <= x] where y = round x

-- g)
factores :: Int -> [Int]; factores 1 = []
factores n 
  | factor == [] = [n] 
  | otherwise = factor ++ factores (n `div` (head factor)) 
    where factor = take 1 (filter (\x -> (n `mod` x) == 0) [2..n-1])
