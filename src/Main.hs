module Main where

import Codec.Picture.Repa (readImageRGBA, toByteString, reverseColorChannel)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Animate (pictures)
import System.Random
import Graphics.Gloss.Data.Bitmap (loadBMP)

data DirecaoMovimento = Esquerda | Direita | Centro | Cima | Baixo deriving (Eq)

data AcaoJogador = Atirando | Nenhum deriving (Eq)

data Tela = Menu | Jogando | GameOver deriving (Eq)

data Bala = Bala { balaX :: Float, balaY :: Float }

data Inimigo = Inimigo { inimigoX :: Float, inimigoY :: Float, proxTiro :: Float, atirando :: Bool}

data World = World {  posicaoX      :: Float
                    , posicaoY      :: Float
                    , direcao       :: DirecaoMovimento 
                    , spriteAtual   :: Int
                    , acaoAtual     :: AcaoJogador 
                    , velocidadeX   :: Float
                    , velocidadeY   :: Float
                    , tela          :: Tela
                    , balas         :: [Bala]
                    , inimigos      :: [Inimigo]
                    , balasInimigos :: [Bala]
                    , timer         :: Float
                    , spawning      :: Bool
                    }

handler :: Event -> World -> World
handler (EventKey (Char 'a') Down _ _) gs = gs { velocidadeX = velocidadeX gs + (-10)}
handler (EventKey (Char 'd') Down _ _) gs = gs { velocidadeX = velocidadeX gs +   10}
handler (EventKey (Char 'w') Down _ _) gs = gs { velocidadeY = velocidadeY gs +   10}
handler (EventKey (Char 's') Down _ _) gs = gs { velocidadeY = velocidadeY gs + (-10)}

handler (EventKey (Char 'a') Up _ _) gs = gs { velocidadeX = velocidadeX gs - (-10)}
handler (EventKey (Char 'd') Up _ _) gs = gs { velocidadeX = velocidadeX gs -   10}
handler (EventKey (Char 'w') Up _ _) gs = gs { velocidadeY = velocidadeY gs -   10}
handler (EventKey (Char 's') Up _ _) gs = gs { velocidadeY = velocidadeY gs - (-10)}

handler (EventKey (SpecialKey KeySpace) Down _ _) gs = disparaBala gs

handler _ gs = gs

janela :: Display 
janela = InWindow "Haskell 1942" (1024, 768) (0, 0)

-- Gera o inimigo na lista de inimigos
spawnInimigo :: World -> World
spawnInimigo gs = gs { spawning = True
                     , inimigos = inimigos gs ++ [Inimigo { inimigoX = 0, inimigoY = 384, proxTiro = 0.5, atirando = False}]}

-- Controle de fluxo do jogo
step :: Float -> World -> World
step t gs = case tela gs of
  Menu     -> gs
  Jogando  -> updateJogando t gs
  GameOver -> gs

-- Cria um novo inimigo a cada 3 segundos
spawner :: Float -> World -> World
spawner t gs = if floor (timer gs) `mod` 3 == 0 then
    if spawning gs then
      gs
    else
      spawnInimigo gs
  else 
    gs { spawning = False }

updateTimer :: Float -> World -> World
updateTimer t gs = gs { timer = timer gs + t}

-- Se o inimigo está pronto para atirar adiciona uma nova bala na lista de balas inimigas
inimigosAtiram :: World -> World
inimigosAtiram gs = gs { balasInimigos = balasInimigos gs ++
                                      [Bala { balaX = inimigoX i, balaY = inimigoY i }
                                            | i <- inimigos gs, proxTiro i < 0, not (atirando i)]}

-- Caso o inimigo já tenha atirado reseto o próximo tiro, do contrário mantenho p
resetaTiro :: Float -> Float
resetaTiro p = if p < -0.1 then 1 else p

-- Roda em loop chamando os updates
updateJogando :: Float -> World -> World
updateJogando t gs = spawner t $ updateWorldState t $ updateTimer t $ inimigosAtiram gs

-- Atualiza todos os estados de forma genérica
updateWorldState :: Float -> World -> World
updateWorldState t gs = gs {
                           posicaoX      = seguraBordas (posicaoX gs + fatorX) 1024
                         , posicaoY      = seguraBordas (posicaoY gs + fatorY) 768
                         , direcao       = handleDirecao
                         , balas         = atualizaBalas
                         , balasInimigos = atualizaBalasInimigos
                         , inimigos      = atualizaInimigos
                         }
                         where fatorX = if velocidadeY gs /= 0 then
                                          velocidadeX gs * 0.7
                                        else
                                          velocidadeX gs
                               fatorY = if velocidadeX gs /= 0 then
                                            velocidadeY gs * 0.7
                                          else
                                            velocidadeY gs
                               atualizaBalas = [Bala { balaX = balaX bala
                                                     , balaY = balaY bala + 25}| bala <- balas gs
                                                                               , balaY bala < 368] -- Tamanho de meia tela
                               atualizaBalasInimigos = [Bala { balaX = balaX bala
                                                             , balaY = balaY bala - 8}| bala <- balasInimigos gs
                                                                                        , balaY bala > -400]
                               atualizaInimigos = [Inimigo { inimigoX = inimigoX i
                                                           , inimigoY = inimigoY i - 3
                                                           , proxTiro = resetaTiro (proxTiro i - t)
                                                           , atirando = proxTiro i < 0 } | i <- inimigos gs, inimigoY i > -400]
                               handleDirecao
                                    | velocidadeX gs == 0 = Centro
                                    | velocidadeX gs < 0  = Esquerda
                                    | otherwise = Direita

-- Garante que o jogador não vai voar para fora da tela
seguraBordas :: Float -> Float -> Float
seguraBordas pos borda
  | pos > (borda / 2)    = borda / 2
  | pos < ((-borda) / 2) = (-borda) / 2
  | otherwise            = pos

-- Cria uma nova bala na lista de balas ativas
disparaBala :: World -> World
disparaBala gs = gs { balas = balas gs ++ [Bala { balaX = posicaoX gs, balaY = posicaoY gs }] }

-- Faz toda a renderização gráfica
draw :: World -> [Picture] -> Picture 
draw gs ps = pictures $ 
              translate posX posY (qualFrame gs ps) :
              [translate (balaX bala) (balaY bala) balaImg | bala <- balas gs] ++
              [translate (balaX bala) (balaY bala) balaImgIn | bala <- balasInimigos gs] ++
              [translate (inimigoX i) (inimigoY i) inimigoImg | i <- inimigos gs]
  where posX       = posicaoX gs
        posY       = posicaoY gs
        balaImg    = ps !! 3
        balaImgIn  = ps !! 5
        inimigoImg = ps !! 4

-- Decide qual é a imagem correta do player dado sua direção
qualFrame :: World -> [Picture] -> Picture
qualFrame gs ps
  | direcao gs == Esquerda = ps !! 1
  | direcao gs == Direita = ps !! 2
  | otherwise = head ps

main :: IO ()
main = do
  pcentro       <- loadPNG "assets/plane_center.png" 87 68 False
  pesquerdo     <- loadPNG "assets/plane_left.png"   87 68 False
  pdireito      <- loadPNG "assets/plane_right.png"  87 68 False
  inimigo       <- loadPNG "assets/inimigo.png"      87 68 False
  bullet        <- loadPNG "assets/bullet.png"       8  16 False
  bulletInimigo <- loadPNG "assets/bullet.png"       8  16 True
  let world = World { 
    posicaoX      = 0.0,
    posicaoY      = 0.0,
    direcao       = Centro,
    spriteAtual   = 0,
    acaoAtual     = Nenhum,
    velocidadeX   = 0,
    velocidadeY   = 0,
    tela          = Jogando,
    balas         = [],
    inimigos      = [],
    balasInimigos = [],
    timer         = 0,
    spawning      = True
  }
  play
    janela
    white
    60
    world
    (`draw` [pcentro, pesquerdo, pdireito, bullet, inimigo, bulletInimigo])
    handler
    step

-- Possibilita carregar PNGs ao invés dos BMP suportados pelo Gloss
-- Referência https://stackoverflow.com/questions/12222728/png-to-bmp-in-haskell-for-gloss
loadPNG :: FilePath -> Int -> Int -> Bool -> IO Picture
loadPNG path w h rot = do
                        (Right img) <- readImageRGBA path
                        let bs = toByteString $ reverseColorChannel img
                        if rot then
                            return $ bitmapOfByteString w h (BitmapFormat BottomToTop PxRGBA) bs True
                        else
                            return $ bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True