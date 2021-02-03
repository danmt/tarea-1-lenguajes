-- Ejercicio 3
-- Autores:
--  - Denyl Romero
--  - Daniel Marin

-- a)
take 5 [4,2..]

-- b)
potencias :: Int -> [Int]
potencias n = [2^x | x <- [1..n]]

-- c)
impares :: Int -> [Int]
impares n = take n [1,3..]

-- d)
replicar :: Int -> a -> [a]
replicar n x = [y | y <- map (const x) [1..n]]

-- e)
caracteres :: Int -> Maybe String
caracteres x = if x > 0 && x < 27 then Just (take x ['A'..]) else Nothing

-- f)
normasMenoresQue :: Float -> [(Int, Int)]
normasMenoresQue x = [(a,b) | a <- [-y..y], b <- [-y..y], sqrt(fromIntegral((a^2 + b^2))) <= x] where y = round x

-- g)
factores :: Int -> [Int]; factores 1 = []
factores n 
  | factor == [] = [n] 
  | otherwise = factor ++ factores (n `div` (head factor)) 
    where factor = take 1 (filter (\x -> (n `mod` x) == 0) [2..n-1])
