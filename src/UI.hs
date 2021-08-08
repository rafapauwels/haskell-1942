module UI where

import Graphics.Gloss
import Numeric.Extra

import World

drawMenu :: World -> Picture -> Picture -> [Picture]
drawMenu gs menu instrucoes = [translate 0 (sin (timer gs * 2) * 20 + 200) menu] ++
                              [translate (sin (timer gs) * 10) (-200) instrucoes]

drawVidas :: World -> Picture -> [Picture]
drawVidas gs vida = [translate (-500 + intToFloat v * 40) (-340) vida | v <- [1 .. (vidas (jogador gs))] ]

drawPontuacao :: World -> [Picture]
drawPontuacao gs = [scale 0.25 0.25 
                  . color (makeColor 0 0 0 1)
                  . translate (-2048) 1400
                  . text $ "Score: " ++ show (score gs)]