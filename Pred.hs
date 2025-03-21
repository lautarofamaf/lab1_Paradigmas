module Pred where

import Dibujo

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
-- A REVISAR FUNCION CAMBIAR ERROR DE TIPOS
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f = foldDib (\x-> if p x then f x else Basica x) rotar rotar45 espejar apilar juntar encimar 
                                             
-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred = foldDib pred id id id (\_ _ d1 d2 -> d1 || d2) (\_ _ d1 d2 -> d1 || d2) (\d1 d2 -> d1 || d2)
-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pred = foldDib pred id id id (\_ _ d1 d2 -> d1 && d2) (\_ _ d1 d2 -> d1 && d2) (\d1 d2 -> d1 && d2)

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 d = foldDib (\x -> 0)
                     (\x -> if x < 4 then x+1 else 4)
                     (\x -> if x == 4 then 4 else 0)
                     (\x -> if x == 4 then 4 else 0)
                     (\_ _ x y -> if x == 4 || y == 4 then 4 else 0)
                     (\_ _ x y -> if x == 4 || y == 4 then 4 else 0)
                     (\x y -> if x == 4 || y == 4 then 4 else 0) d == 4

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 d = foldDib  (\x -> 0)
                     (\x -> if x == 2 then 2 else 0)
                     (\x -> if x < 2 then x+1 else 2)
                     (\x -> if x == 2 then 2 else 0)
                     (\_ _ x y -> if x == 2 || y == 2 then 2 else 0)
                     (\_ _ x y -> if x == 2 || y == 2 then 2 else 0)
                     (\x y -> if x == 2 || y == 2 then 2 else 0) d == 2

data Superfluo = RotacionSuperflua | FlipSuperfluo

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion d = [RotacionSuperflua | esRot360 d]
-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip d = [FlipSuperfluo | esFlip2 d]

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo d =
  let errores = errorRotacion d ++ errorFlip d
  in if null errores
     then Right d
     else Left errores
