-- Ejercicio 2
-- Autores:
--  - Denyl Romero
--  - Daniel Marin

listDigits :: Int -> [Int]
listDigits x = if x <= 0 then [] else listDigits (x `div` 10) ++ [x `mod` 10]

listDigitsRev :: Int -> [Int]
listDigitsRev x = if x <= 0 then [] else x `mod` 10 : listDigitsRev (x `div` 10)