module Background where

import Graphics.Gloss

import World

-- | Desenha o plano de fundo do jogo, alterando a cada segundo o frame usado para representar o oceano
-- | Mapas mais complexos podem ser desenhados aqui e concatenados a lista final de Picture
drawBackground :: World -> Picture -> Picture -> [Picture]
drawBackground gs a1 a2 = [translate x y (qualAgua gs a1 a2) | x <- [-512, -456 .. 512], y <- [-384, -327 .. 384]]

-- | Retorna qual é o frame a ser mostrado de acordo com tempo passado dentro do jogo
qualAgua :: World -> Picture -> Picture -> Picture
qualAgua gs p1 p2 = if even (floor (timer gs)) then p1 else p2