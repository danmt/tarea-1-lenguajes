-- Ejercicio 1
-- Autores:
--  - Denyl Romero
--  - Daniel Marin

type Conjunto a = a -> Bool

-- a)
miembro :: Conjunto a -> a -> Bool
miembro esMiembro elemento = esMiembro elemento

-- b)
vacio :: Conjunto a
vacio esMiembro = False

-- c)
singleton :: (Eq a) => a -> Conjunto a
singleton x = \ x' -> x == x'

-- d)
desdeLista :: (Eq a) => [a] -> Conjunto a
desdeLista [] = vacio
desdeLista (xs:x) = \ x' -> singleton xs x' || desdeLista x x'

-- e)
complemento :: (Eq a) => a -> Conjunto a
complemento x = \ x' -> x /= x'

-- f)
union :: Conjunto a -> Conjunto a -> Conjunto a
union conjunto1 conjunto2 = \ x -> miembro conjunto1 x || miembro conjunto2 x

-- g)
interseccion :: Conjunto a -> Conjunto a -> Conjunto a
interseccion conjunto1 conjunto2 = \ x -> miembro conjunto1 x && miembro conjunto2 x

-- h)
diferencia :: Conjunto a -> Conjunto a -> Conjunto a
diferencia conjunto1 conjunto2 = \ x -> miembro conjunto1 x && not (miembro conjunto2 x)

-- i)
diferenciaSimetrica :: Conjunto a -> Conjunto a -> Conjunto a
diferenciaSimetrica conjunto1 conjunto2 = 
  \ x -> union (diferencia conjunto1 conjunto2) (diferencia conjunto2 conjunto1) x

-- j)
cartesiano :: Conjunto a -> Conjunto a -> Conjunto (a,a)
cartesiano conjunto1 conjunto2 = 
  \ (x,y) -> miembro conjunto1 x && miembro conjunto2 y

-- k)
transformar :: (b -> a) -> Conjunto a -> Conjunto b
transformar funcion conjunto = \ x -> miembro conjunto (funcion x)