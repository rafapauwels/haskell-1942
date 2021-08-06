module Main where

import Codec.Picture.Repa (readImageRGBA, toByteString, reverseColorChannel)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Animate (pictures)

data DirecaoMovimento = Esquerda | Direita | Centro | Cima | Baixo deriving (Eq)

data AcaoJogador = Atirando | Nenhum deriving (Eq)

data World = World {  posicaoX    :: Float
                    , posicaoY    :: Float
                    , direcao     :: DirecaoMovimento 
                    , spriteAtual :: Int
                    , acaoAtual   :: AcaoJogador 
                    , velocidadeX :: Float
                    , velocidadeY :: Float
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
update _ gs = gs { 
                   posicaoX = posicaoX gs + velocidadeX gs
                 , posicaoY = posicaoY gs + velocidadeY gs
                 }

draw :: World -> [Picture] -> Picture 
draw gs ps = translate posX posY (head ps)
  where posX = posicaoX gs 
        posY = posicaoY gs

main :: IO ()
main = do
  plane <- loadPNG "assets/plane_center.png"
  let world = World { 
    posicaoX    = 0.0,
    posicaoY    = 0.0,
    direcao     = Centro,
    spriteAtual = 0,
    acaoAtual   = Nenhum,
    velocidadeX = 0,
    velocidadeY = 0
  }
  play
    janela
    white
    60
    world
    (`draw` [plane])
    handler
    update


-- Referência https://stackoverflow.com/questions/12222728/png-to-bmp-in-haskell-for-gloss
loadPNG :: FilePath -> IO Picture
loadPNG path = do
  (Right img) <- readImageRGBA path
  let bs = toByteString $ reverseColorChannel img
  return $ bitmapOfByteString 87 68 (BitmapFormat TopToBottom PxRGBA) bs True