module Collider where

import World

collider :: World -> World
collider gs = gs -- colliderInimigoJogador gs

-- colliderInimigoJogador :: World -> World
-- colliderInimigoJogador gs = [colidiu jogadorX (jogador gs) jogadorY (jogador gs) ]