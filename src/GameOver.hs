module GameOver where

import Graphics.Gloss

import World

drawGameOver :: World -> Picture -> [Picture]
drawGameOver gs p = if tela gs == GameOver then
                      if even (floor (timer gs)) then
                        [p]
                      else
                        []
                    else
                      []