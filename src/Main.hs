module Main where

import Codec.Picture.Repa (readImageRGBA, toByteString, reverseColorChannel)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Animate (pictures)

data DirecaoMovimento = Esquerda | Direita | Centro | Cima | Baixo deriving (Eq)

data AcaoJogador = Atirando | Nenhum deriving (Eq)

data Tela = Menu | Jogando | GameOver deriving (Eq)

data World = World {  posicaoX    :: Float
                    , posicaoY    :: Float
                    , direcao     :: DirecaoMovimento 
                    , spriteAtual :: Int
                    , acaoAtual   :: AcaoJogador 
                    , velocidadeX :: Float
                    , velocidadeY :: Float
                    , tela        :: Tela
                    }

janela :: Display 
janela = InWindow "Haskell 1942" (1024, 768) (0, 0)

handler :: Event -> World -> World
handler (EventKey (Char 'a') Down _ _) gs = gs { velocidadeX = velocidadeX gs + (-10)}
handler (EventKey (Char 'd') Down _ _) gs = gs { velocidadeX = velocidadeX gs +   10}
handler (EventKey (Char 'w') Down _ _) gs = gs { velocidadeY = velocidadeY gs +   10}
handler (EventKey (Char 's') Down _ _) gs = gs { velocidadeY = velocidadeY gs + (-10)}

handler (EventKey (Char 'a') Up _ _) gs = gs { velocidadeX = velocidadeX gs - (-10)}
handler (EventKey (Char 'd') Up _ _) gs = gs { velocidadeX = velocidadeX gs -   10}
handler (EventKey (Char 'w') Up _ _) gs = gs { velocidadeY = velocidadeY gs -   10}
handler (EventKey (Char 's') Up _ _) gs = gs { velocidadeY = velocidadeY gs - (-10)}
handler _ gs = gs

update :: Float -> World -> World
update _ gs
  | tela gs == Jogando = updateJogo gs
  | otherwise          = gs

updateJogo :: World -> World
updateJogo gs = gs {
                   posicaoX = seguraBordas (posicaoX gs + fatorX) 1024
                 , posicaoY = seguraBordas (posicaoY gs + fatorY) 768
                 , direcao  = handleDirecao
                 }
                 where fatorX = if velocidadeY gs /= 0 then
                                  velocidadeX gs * 0.7
                                else
                                  velocidadeX gs
                       fatorY = if velocidadeX gs /= 0 then
                                  velocidadeY gs * 0.7
                                else
                                  velocidadeY gs

                       handleDirecao
                          | velocidadeX gs == 0 = Centro
                          | velocidadeX gs < 0  = Esquerda
                          | otherwise = Direita

seguraBordas :: Float -> Float -> Float
seguraBordas pos borda
  | pos > (borda / 2)    = borda / 2
  | pos < ((-borda) / 2) = (-borda) / 2
  | otherwise            = pos

draw :: World -> [Picture] -> Picture 
draw gs ps = translate posX posY (qualFrame gs ps)
  where posX = posicaoX gs
        posY = posicaoY gs

qualFrame :: World -> [Picture] -> Picture
qualFrame gs ps
  | direcao gs == Esquerda = ps !! 1
  | direcao gs == Direita = ps !! 2
  | otherwise = head ps

main :: IO ()
main = do
  pcentro   <- loadPNG "assets/plane_center.png"
  pesquerdo <- loadPNG "assets/plane_left.png"
  pdireito  <- loadPNG "assets/plane_right.png"
  let world = World { 
    posicaoX    = 0.0,
    posicaoY    = 0.0,
    direcao     = Centro,
    spriteAtual = 0,
    acaoAtual   = Nenhum,
    velocidadeX = 0,
    velocidadeY = 0,
    tela        = Jogando
  }
  play
    janela
    white
    60
    world
    (`draw` [pcentro, pesquerdo, pdireito])
    handler
    update


-- Referência https://stackoverflow.com/questions/12222728/png-to-bmp-in-haskell-for-gloss
loadPNG :: FilePath -> IO Picture
loadPNG path = do
  (Right img) <- readImageRGBA path
  let bs = toByteString $ reverseColorChannel img
  return $ bitmapOfByteString 87 68 (BitmapFormat TopToBottom PxRGBA) bs True