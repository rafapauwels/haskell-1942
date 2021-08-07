module World where

data DirecaoMovimento = Esquerda | Direita | Centro | Cima | Baixo deriving (Eq)

data AcaoJogador = Atirando | Nenhum deriving (Eq)

data Tela = Menu | Jogando | GameOver deriving (Eq)

data Bala = Bala { balaX :: Float, balaY :: Float }

data Jogador = Jogador { jogadorX :: Float, jogadorY :: Float, direcao :: DirecaoMovimento, spriteAtual :: Int, acaoAtual :: AcaoJogador, velocidadeX :: Float, velocidadeY :: Float, vidas :: Int}

data Inimigo = Inimigo { inimigoX :: Float, inimigoY :: Float, proxTiro :: Float, atirando :: Bool}

data World = World {  jogador       :: Jogador
                    , tela          :: Tela
                    , balas         :: [Bala]
                    , inimigos      :: [Inimigo]
                    , balasInimigos :: [Bala]
                    , timer         :: Float
                    , spawning      :: Bool
                    }