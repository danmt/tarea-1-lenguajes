-- Ejercicio 2
-- Autores:
--  - Denylson Romero
--  - Daniel Marin

listDigits :: Int -> [Int]
listDigits x 
  | x <= 0 = []
  | otherwise = listDigits (div x 10) ++ [mod x 10]

listDigitsRev :: Int -> [Int]
listDigitsRev x 
  | x <= 0 = [] 
  | otherwise = mod x 10 : listDigitsRev (div x 10)