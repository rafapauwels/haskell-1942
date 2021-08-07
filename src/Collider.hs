module Collider where

import World

collider :: World -> World
collider gs = colliderInimigoJogador gs

colliderInimigoJogador :: World -> World
colliderInimigoJogador gs = gs { jogador = Jogador { jogadorX    = jogadorX (jogador gs)
                                                   , jogadorY    = jogadorY (jogador gs)
                                                   , direcao     = direcao (jogador gs)
                                                   , spriteAtual = spriteAtual (jogador gs)
                                                   , acaoAtual   = acaoAtual (jogador gs)
                                                   , velocidadeX = velocidadeX (jogador gs)
                                                   , velocidadeY = velocidadeY (jogador gs)
                                                   , vidas       = if length (colisoesComInimigos gs) == 0 then 1 else 0 }}

                                                  --  , vidas       = if length (colisoesComInimigos gs) == 0 then vidas (jogador gs) else vidas (jogador gs) - 1 }}

colisoesComInimigos :: World -> [Inimigo]
colisoesComInimigos gs = [ i | i <- inimigos gs, 
                            (inimigoX i - 40 < (jogadorX (jogador gs) + 15) && inimigoX i + 40 > (jogadorX (jogador gs) - 15)) &&
                            (inimigoY i - 40 < (jogadorY (jogador gs) + 15) && inimigoY i + 40 > (jogadorY (jogador gs) - 15)) ]