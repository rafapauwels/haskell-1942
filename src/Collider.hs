module Collider where

import World

collider :: World -> World
collider gs = colliderG comInimigos $ colliderG comBalasInimigas $ colliderInimigos gs

colliderInimigos :: World -> World
colliderInimigos gs = if length (balas gs) == 0 then gs else
                              gs { inimigos = filter (\i -> checaColisaoBala i (balas gs)) (inimigos gs) }

checaColisaoBala :: Inimigo -> [Bala] -> Bool
checaColisaoBala i bs = length [i | b <- bs, (
                                                (balaX b - 10 < inimigoX i + 35) && (balaX b + 10 > inimigoX i - 35) && 
                                                (balaY b - 10 < inimigoY i + 35) && (balaY b + 10 > inimigoY i - 35)
                                             )] == 0

colliderG :: (World -> [a]) -> World  -> World
colliderG f gs = if tempoRecuperar (jogador gs) > 0 then
                   gs
                 else
                   gs { jogador = Jogador { jogadorX       = jogadorX (jogador gs)
                                          , jogadorY       = jogadorY (jogador gs)
                                          , direcao        = direcao (jogador gs)
                                          , spriteAtual    = spriteAtual (jogador gs)
                                          , tempoRecuperar = if length (f gs) == 0 then 0 else 3
                                          , velocidadeX    = velocidadeX (jogador gs)
                                          , velocidadeY    = velocidadeY (jogador gs)
                                          , vidas          = if length (f gs) == 0 then vidas (jogador gs) else vidas (jogador gs) - 1 }}

comBalasInimigas :: World -> [Bala]
comBalasInimigas gs = [b | b <- balasInimigos gs,
                            (balaX b - 10 < (jogadorX (jogador gs) + 25) && balaX b + 10 > (jogadorX (jogador gs) - 25)) &&
                            (balaY b - 10 < (jogadorY (jogador gs) + 25) && balaY b + 10 > (jogadorY (jogador gs) - 25)) ]

comInimigos :: World -> [Inimigo]
comInimigos gs = [ i | i <- inimigos gs, 
                            (inimigoX i - 40 < (jogadorX (jogador gs) + 15) && inimigoX i + 40 > (jogadorX (jogador gs) - 15)) &&
                            (inimigoY i - 40 < (jogadorY (jogador gs) + 15) && inimigoY i + 40 > (jogadorY (jogador gs) - 15)) ]