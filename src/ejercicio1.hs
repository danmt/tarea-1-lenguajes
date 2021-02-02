type Conjunto a = a -> Bool

miembro :: Conjunto a -> a -> Bool
miembro esMiembro elemento = esMiembro elemento

vacio :: Conjunto a
vacio esMiembro = False

union :: Conjunto a -> Conjunto a -> Conjunto a
union conjunto1 conjunto2 = \ x -> miembro conjunto1 x || miembro conjunto2 x

interseccion :: Conjunto a -> Conjunto a -> Conjunto a
interseccion conjunto1 conjunto2 = \ x -> miembro conjunto1 x && miembro conjunto2 x

diferencia :: Conjunto a -> Conjunto a -> Conjunto a
diferencia conjunto1 conjunto2 = \ x -> miembro conjunto1 x && not (miembro conjunto2 x)

diferenciaSimetrica :: Conjunto a -> Conjunto a -> Conjunto a
diferenciaSimetrica conjunto1 conjunto2 = 
  \ x -> union (diferencia conjunto1 conjunto2) (diferencia conjunto2 conjunto1) x

cartesiano :: Conjunto a -> Conjunto a -> Conjunto (a,a)
cartesiano conjunto1 conjunto2 = 
  \ (x,y) -> miembro conjunto1 x && miembro conjunto2 y
