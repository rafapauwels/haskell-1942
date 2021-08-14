module World where

data DirecaoMovimento = Esquerda | Direita | Centro | Cima | Baixo deriving (Eq)

data Tela = Menu | Jogando | GameOver deriving (Eq)

data Bala = Bala { balaX :: Float, balaY :: Float }

data Jogador = Jogador { jogadorX :: Float, jogadorY :: Float, direcao :: DirecaoMovimento, spriteAtual :: Int, tempoRecuperar :: Float, velocidadeX :: Float, velocidadeY :: Float, vidas :: Int}

data Inimigo = Inimigo { inimigoX :: Float, inimigoY :: Float, proxTiro :: Float, atirando :: Bool}

-- | Modelo do GameState, atualizado a cada frame
data World = World {  jogador       :: Jogador
                    , tela          :: Tela
                    , balas         :: [Bala]
                    , inimigos      :: [Inimigo]
                    , balasInimigos :: [Bala]
                    , timer         :: Float
                    , spawning      :: Bool
                    , score         :: Int
                    }