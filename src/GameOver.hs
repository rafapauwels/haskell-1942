module GameOver where

import Graphics.Gloss

import World

-- | Gera uma lista de Picture com o conteudo da tela de GameOver
drawGameOver :: World -> Picture -> [Picture]
drawGameOver gs p = [p | tela gs == GameOver && even (floor (timer gs))]