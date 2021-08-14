module Collider where

import World

-- | Executado a cada frame
-- | Verifica cada tipo possível de colisão e atualiza o game state caso alguma colisão ocorra
collider :: World -> World
collider gs = colliderG comInimigos $ colliderG comBalasInimigas $ colliderInimigos gs

-- | Verifica a colisão das balas do jogador com os aviões inimigos
colliderInimigos :: World -> World
colliderInimigos gs = if null (balas gs) then 
                        gs
                      else
                        gs { inimigos = filter (\i -> checaColisaoBala i (balas gs)) (inimigos gs) -- Removo inimigos que colidiram da minha lista de inimigos
                           , score = if any (\i -> not (checaColisaoBala i (balas gs))) (inimigos gs) then score gs + 50 else score gs }

-- | Auxiliar do colliderInimigos
checaColisaoBala :: Inimigo -> [Bala] -> Bool
checaColisaoBala i bs = null [i | b <- bs, (balaX b - 10 < inimigoX i + 35) && (balaX b + 10 > inimigoX i - 35) && 
                                              (balaY b - 10 < inimigoY i + 35) && (balaY b + 10 > inimigoY i - 35)]

-- | Collider genérico que verifica se o jogador colidiu com algum objeto da lista gs
colliderG :: (World -> [a]) -> World  -> World
colliderG f gs = if tempoRecuperar (jogador gs) > 0 then
                   gs
                 else
                   gs { jogador = Jogador { jogadorX       = jogadorX (jogador gs)
                                          , jogadorY       = jogadorY (jogador gs)
                                          , direcao        = direcao (jogador gs)
                                          , spriteAtual    = spriteAtual (jogador gs)
                                          , tempoRecuperar = if null (f gs) then 0 else 3
                                          , velocidadeX    = velocidadeX (jogador gs)
                                          , velocidadeY    = velocidadeY (jogador gs)
                                          , vidas          = if null (f gs) then vidas (jogador gs) else vidas (jogador gs) - 1 }}

-- | Usado em conjunto com o colliderG, delimita a caixa ao redor do centro dos objetos que caracteriza uma colisão
comBalasInimigas :: World -> [Bala]
comBalasInimigas gs = [b | b <- balasInimigos gs,
                            (balaX b - 10 < (jogadorX (jogador gs) + 25) && balaX b + 10 > (jogadorX (jogador gs) - 25)) &&
                            (balaY b - 10 < (jogadorY (jogador gs) + 25) && balaY b + 10 > (jogadorY (jogador gs) - 25)) ]

-- | Usado em conjunto com o colliderG, delimita a caixa ao redor do centro dos objetos que caracteriza uma colisão
comInimigos :: World -> [Inimigo]
comInimigos gs = [ i | i <- inimigos gs, 
                            (inimigoX i - 40 < (jogadorX (jogador gs) + 15) && inimigoX i + 40 > (jogadorX (jogador gs) - 15)) &&
                            (inimigoY i - 40 < (jogadorY (jogador gs) + 15) && inimigoY i + 40 > (jogadorY (jogador gs) - 15)) ]