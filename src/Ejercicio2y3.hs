-- Ejercicio 2
listDigits :: Int -> [Int]; listDigits x = if x <= 0 then [] else listDigits (x `div` 10) ++ [x `mod` 10]

listDigitsRev :: Int -> [Int]; listDigitsRev x = if x <= 0 then [] else x `mod` 10 : listDigitsRev (x `div` 10)

-- Ejercicio 3
-- a)
take 5 [4,2..]

-- b)
potencias :: Int -> [Int]; potencias n = [2^x | x <- [1..n]]

-- c)
impares :: Int -> [Int]; impares n = take n [1,3..]

-- d)
replicar :: Int -> a -> [a]; replicar n x = [y | y <- map (const x) [1..n]]

-- e)
caracteres :: Int -> Maybe String; caracteres x = if x > 0 && x < 27 then Just (take x ['A'..]) else Nothing

-- f) NOTA: Podria ser menor igual o menor estricto, falta comprobar eso y que devuelva int o float. error en enunciado
normasMenoresQue x = [(a,b) | a <- [-x..x], b <- [-x..x], sqrt(a**2 + b**2) < x]

-- g) Me falta la g
