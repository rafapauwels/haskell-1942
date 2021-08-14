module Main where

import Codec.Picture.Repa (readImageRGBA, toByteString, reverseColorChannel)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Background
import World
import Collider
import GameOver
import UI

-- | Executado todo frame
-- | Atualiza o game state de acordo a tecla pressionada
handler :: Event -> World -> World
handler (EventKey (Char 'a') Down _ _) gs = updateVelocidadeX gs (-10)
handler (EventKey (Char 'd') Down _ _) gs = updateVelocidadeX gs 10
handler (EventKey (Char 'w') Down _ _) gs = updateVelocidadeY gs 10
handler (EventKey (Char 's') Down _ _) gs = updateVelocidadeY gs (-10)

handler (EventKey (Char 'a') Up _ _) gs = updateVelocidadeX gs 10
handler (EventKey (Char 'd') Up _ _) gs = updateVelocidadeX gs (-10)
handler (EventKey (Char 'w') Up _ _) gs = updateVelocidadeY gs (-10)
handler (EventKey (Char 's') Up _ _) gs = updateVelocidadeY gs 10

handler (EventKey (SpecialKey KeySpace) Down _ _) gs = disparaBala gs

handler (EventKey (SpecialKey KeyEnter) Down _ _) gs = if tela gs == Menu then resetState Jogando else gs
handler (EventKey (Char 'r') Down _ _) gs      = if tela gs == GameOver then resetState Jogando else gs

handler _ gs = gs

-- | Limpa o game state, voltando todas as variáveis para seu estado inicial
resetState :: Tela -> World
resetState t = World {
                      tela          = t
                    , balas         = []
                    , inimigos      = []
                    , balasInimigos = []
                    , timer         = 0
                    , spawning      = True
                    , score         = 0
                    , jogador       = Jogador { jogadorX       = 0.0
                                              , jogadorY       = 0.0
                                              , direcao        = Centro
                                              , spriteAtual    = 0
                                              , tempoRecuperar = 0
                                              , velocidadeX    = 0
                                              , velocidadeY    = 0
                                              , vidas          = 3 }
                  }

updateVelocidadeX :: World -> Float -> World
updateVelocidadeX gs v = gs { jogador = Jogador { jogadorX       = jogadorX (jogador gs)
                                                , jogadorY       = jogadorY (jogador gs)
                                                , direcao        = direcao (jogador gs)
                                                , spriteAtual    = spriteAtual (jogador gs)
                                                , tempoRecuperar = tempoRecuperar (jogador gs)
                                                , velocidadeX    = velocidadeX (jogador gs) + v
                                                , velocidadeY    = velocidadeY (jogador gs)
                                                , vidas          = vidas (jogador gs) }}

updateVelocidadeY :: World -> Float -> World
updateVelocidadeY gs v = gs { jogador = Jogador { jogadorX       = jogadorX (jogador gs)
                                                , jogadorY       = jogadorY (jogador gs)
                                                , direcao        = direcao (jogador gs)
                                                , spriteAtual    = spriteAtual (jogador gs)
                                                , tempoRecuperar = tempoRecuperar (jogador gs)
                                                , velocidadeX    = velocidadeX (jogador gs)
                                                , velocidadeY    = velocidadeY (jogador gs) + v
                                                , vidas          = vidas (jogador gs) }}

janela :: Display 
janela = InWindow "Haskell 1942" (1024, 768) (0, 0)

-- | Gera o inimigo na lista de inimigos entre -400 e 400 no eixo X
spawnInimigo :: World -> World
spawnInimigo gs = gs { spawning = True
                     , inimigos = inimigos gs ++ [Inimigo { inimigoX = sin (timer gs) * 400, inimigoY = 384, proxTiro = 0.5, atirando = False}]}

-- | Controle de fluxo do jogo
step :: Float -> World -> World
step t gs = case tela gs of
  Menu     -> updateTimer t gs   -- Nao tem atualizaçao
  Jogando  -> updateJogando t gs -- Jogo normal
  GameOver -> updateTimer t gs   -- Congela a tela

-- | Cria um novo inimigo a cada segundo
spawner :: Float -> World -> World
spawner t gs = if even (floor (timer gs)) then
    if spawning gs then
      gs
    else
      spawnInimigo gs
  else 
    gs { spawning = False }

-- | Executado a cada frame, incrementa a variável timer com o tempo gasto entre dois frames t
updateTimer :: Float -> World -> World
updateTimer t gs = gs { timer = timer gs + t}

-- | Se o inimigo está pronto para atirar adiciona uma nova bala na lista de balas inimigas
inimigosAtiram :: World -> World
inimigosAtiram gs = gs { balasInimigos = balasInimigos gs ++
                                      [Bala { balaX = inimigoX i, balaY = inimigoY i }
                                            | i <- inimigos gs, proxTiro i < 0, not (atirando i)]}

-- | Caso o inimigo já tenha atirado reseto o próximo tiro, do contrário mantenho p
resetaTiro :: Float -> Float
resetaTiro p = if p < -0.1 then 1 else p

-- | Roda em loop chamando os updates
updateJogando :: Float -> World -> World
updateJogando t gs = spawner t $ updateWorldState t $ updateTimer t $ inimigosAtiram $ collider gs

-- | Atualiza todos os estados de forma genérica
updateWorldState :: Float -> World -> World
updateWorldState t gs = gs {
                           jogador       = atualizaJogador
                         , balas         = atualizaBalas
                         , balasInimigos = atualizaBalasInimigos
                         , inimigos      = atualizaInimigos
                         , tela          = if vidas (jogador gs) == 0 then GameOver else Jogando
                         }
                         where fatorX = if velocidadeY (jogador gs) /= 0 then
                                          velocidadeX (jogador gs) * 0.7
                                        else
                                          velocidadeX $ jogador gs
                               fatorY = if velocidadeX (jogador gs) /= 0 then
                                            velocidadeY (jogador gs) * 0.7
                                          else
                                            velocidadeY $ jogador gs
                               atualizaBalas = [Bala { balaX = balaX bala
                                                     , balaY = balaY bala + 25}| bala <- balas gs
                                                                               , balaY bala < 368] -- Tamanho de meia tela
                               atualizaBalasInimigos = [Bala { balaX = balaX bala
                                                             , balaY = balaY bala - 10}| bala <- balasInimigos gs
                                                                                        , balaY bala > -400]
                               atualizaInimigos = [Inimigo { inimigoX = sin (timer gs) * 2 + inimigoX i
                                                           , inimigoY = inimigoY i - 5
                                                           , proxTiro = resetaTiro (proxTiro i - t)
                                                           , atirando = proxTiro i < 0 } | i <- inimigos gs, inimigoY i > -400]
                               atualizaJogador = Jogador { jogadorX       = seguraBordas (jogadorX (jogador gs) + fatorX) 1024
                                                         , jogadorY       = seguraBordas (jogadorY (jogador gs) + fatorY) 768
                                                         , direcao        = handleDirecao
                                                         , spriteAtual    = spriteAtual (jogador gs)
                                                         , tempoRecuperar = if tempoRecuperar (jogador gs) > 0 then tempoRecuperar (jogador gs) - t else 0
                                                         , velocidadeX    = velocidadeX (jogador gs)
                                                         , velocidadeY    = velocidadeY (jogador gs)
                                                         , vidas          = vidas (jogador gs) }
                               handleDirecao
                                    | velocidadeX (jogador gs) == 0 = Centro
                                    | velocidadeX (jogador gs) < 0  = Esquerda
                                    | otherwise = Direita

-- | Garante que o jogador não vai voar para fora da tela
seguraBordas :: Float -> Float -> Float
seguraBordas pos borda
  | pos >= (borda / 2)    = borda / 2
  | pos <= ((-borda) / 2) = (-borda) / 2
  | otherwise             = pos

-- | Cria uma nova bala na lista de balas ativas
disparaBala :: World -> World
disparaBala gs = if tela gs == Jogando then
                   gs { balas = balas gs ++ [Bala { balaX = jogadorX (jogador gs), balaY = jogadorY (jogador gs) }] }
                 else
                   gs

-- | Executado a cada frame
-- | Faz toda a renderização gráfica
draw :: World -> [Picture] -> Picture 
draw gs ps = pictures $ 
              drawBackground gs agua1 agua2 ++
              if tela gs == Menu then
                drawMenu gs menu instrucoes
              else
                blink gs (translate posX posY (qualFrame gs ps)) ++
                [translate (balaX bala) (balaY bala) balaImg | bala <- balas gs] ++
                [translate (balaX bala) (balaY bala) balaImgIn | bala <- balasInimigos gs] ++
                [translate (inimigoX i) (inimigoY i) inimigoImg | i <- inimigos gs] ++
                drawVidas gs vida ++
                drawPontuacao gs ++
                drawGameOver gs gameover
  where posX       = jogadorX $ jogador gs
        posY       = jogadorY $ jogador gs
        balaImg    = ps !! 3
        balaImgIn  = ps !! 5
        inimigoImg = ps !! 4
        agua1      = ps !! 6
        agua2      = ps !! 7
        gameover   = ps !! 8
        menu       = ps !! 9
        instrucoes = ps !! 10
        vida       = ps !! 11

-- | Pisca jogador caso ele esteja se recuperando de um hit
blink :: World -> Picture -> [Picture]
blink gs p = if tempoRecuperar (jogador gs) > 0 then [p | (timer gs - fromIntegral (floor (timer gs))) / 4 > 0.1] else [p]

-- | Decide qual é a imagem correta do player dado sua direção de movimento
qualFrame :: World -> [Picture] -> Picture
qualFrame gs ps
  | direcao (jogador gs) == Esquerda = ps !! 1
  | direcao (jogador gs) == Direita  = ps !! 2
  | otherwise = head ps

-- | Inicia o game loop e configura quais serão as funções responsáveis por renderizar
-- | e por tratar o game state a cada frame.
main :: IO ()
main = do
  pcentro       <- loadPNG "assets/plane_center.png" 87  68  False
  pesquerdo     <- loadPNG "assets/plane_left.png"   87  68  False
  pdireito      <- loadPNG "assets/plane_right.png"  87  68  False
  inimigo       <- loadPNG "assets/inimigo.png"      87  68  False
  bullet        <- loadPNG "assets/bullet.png"       16  32  False
  bulletInimigo <- loadPNG "assets/bullet.png"       16  32  True
  agua1         <- loadPNG "assets/agua_1.png"       56  57  False
  agua2         <- loadPNG "assets/agua_2.png"       56  57  False
  gameover      <- loadPNG "assets/gameover.png"     400 80  False
  titulo        <- loadPNG "assets/titulo.png"       486 41  False
  instrucoes    <- loadPNG "assets/instrucoes.png"   444 280 False
  vida          <- loadPNG "assets/vida.png"         30  30  False
  let world = resetState Menu
  play
    janela
    white
    60
    world
    (`draw` [pcentro, pesquerdo, pdireito, bullet, inimigo, bulletInimigo, agua1, agua2, gameover, titulo, instrucoes, vida])
    handler
    step

-- | Possibilita carregar PNGs ao invés dos BMP suportados pelo Gloss
-- | Referência https://stackoverflow.com/questions/12222728/png-to-bmp-in-haskell-for-gloss
loadPNG :: FilePath -> Int -> Int -> Bool -> IO Picture
loadPNG path w h rot = do
                        (Right img) <- readImageRGBA path
                        let bs = toByteString $ reverseColorChannel img
                        if rot then
                            return $ bitmapOfByteString w h (BitmapFormat BottomToTop PxRGBA) bs True
                        else
                            return $ bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True