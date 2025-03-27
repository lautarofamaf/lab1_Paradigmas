-- Sacar del esqueleto final!
module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar d w h = (rotate -90) d+h h -w

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar d w h = (scale -1 1) d+w -w h
-- espejar(f)(d, w, h)
-- f(d+w, -w, h)

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 = rotate 45 (d w h)

--interpreta el operador de apilar
interp_apilar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m (d w h) (d1 w1 h1) =  

--interpreta el operador de juntar
interp_juntar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante

--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp inte d = foldDib
