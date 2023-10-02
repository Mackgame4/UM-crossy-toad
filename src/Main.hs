{- |
Module      : Main
Description : Executa a parte gráfica do jogo
Copyright   : Fábio Magalhães <a104365@alunos.uminho.pt>
              Pedro Rosário <a100536@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.

__Nota:__ @Para a realização desta tarefa, foram importadas todas as tarefas anteriores bem como algumas bibliotecas complementares ao random, System e Gloss.@

== Bibliotecas utilizadas:
> import System.Random -- ^ Importa a biblioteca random (para a geração de números aleatórios)
> import Graphics.Gloss -- ^ Importa o Gloss (para a execução da parte gráfica do jogo)
> import Graphics.Gloss.Interface.IO.Game -- ^ Importa a função "playIO" do Gloss (para a execução do jogo)
> import Graphics.Gloss.Interface.Environment -- ^ Importa a função "getScreenSize" do Gloss que retorna um IO (Int,Int) com o tamanho do ecrã
> import System.IO.Unsafe -- ^ Importa a função "unsafePerformIO" do System.IO.Unsafe (não é recomendado o uso desta função, mas foi utilizada para obter o tamanho do ecrã apenas para o inicio do jogo)
> import System.Exit -- ^ Importa a função "exitSuccess" do System.Exit (para terminar o jogo em segurança)

==== Instalação das bibliotecas:
> $ cabal install --lib random
> $ cabal install --lib gloss
-}
module Main where

import LI12223
import Tarefa1_2022li1g009
import Tarefa2_2022li1g009
import Tarefa3_2022li1g009
import Tarefa4_2022li1g009
import Tarefa5_2022li1g009

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game -- importar a função "playIO" do Gloss
--import Graphics.Gloss.Interface.Pure.Game -- importar a função "play" do Gloss
import Graphics.Gloss.Interface.Environment -- importa a função "getScreenSize" do Gloss que retorna um IO (Int,Int) com o tamanho do ecrã
import System.IO.Unsafe -- importar a função "unsafePerformIO" do System.IO.Unsafe
import System.Exit -- importar a função "exitSuccess" do System.Exit

type Score = Int -- ^ Tipo que define o score do jogo
type Difficulty = Int -- ^ Tipo que define a dificuldade do jogo
type JogadorBMP = Int -- ^ Tipo que define o BMP do jogador selecionado
type SubMenu = Int -- ^ Tipo que define o sub-menu selecionado
type Menu = (Bool, Int, SubMenu, Score, Score, Float, Difficulty, JogadorBMP) -- ^ Tipo que define o menu do jogo (tuplo de tipos anterioremente definidos)
type Estado = (Menu, Jogo) -- ^ Tipo que define o estado do jogo (conjunto do menu e do jogo)

-- | Função 'carregarBMP' que carrega uma imagem BMP nas dimensões definidas pelo 'tamanhobmp' para o Gloss.
carregarBMP :: String -- ^ Caminho do ficheiro BMP
                -> Picture -- ^ Retorna a imagem BMP em formato Picture utilizável pelo Gloss
carregarBMP ficheiro = (Translate ((fromIntegral tamanhobmp)/2) ((fromIntegral tamanhobmp)/2) (unsafePerformIO (loadBMP ficheiro)))

-- | Função 'relvabmp' que retorna o BMP da 'Relva' em formato 'Picture'.
relvabmp :: Picture 
relvabmp = carregarBMP "src/assets/relva.bmp"

-- | Função 'aguabmp' que retorna o BMP da água do 'Rio' em formato 'Picture'.
aguabmp :: Picture 
aguabmp = carregarBMP "src/assets/agua.bmp"

-- | Função 'estradabmp' que retorna o BMP da 'Estrada' em formato 'Picture'.
estradabmp :: Picture
estradabmp = carregarBMP "src/assets/estrada.bmp"

{- |
Função 'jogadorbmp' que retorna o BMP do 'Jogador' em formato 'Picture' dado o número do jogador selecionado.

=== __Alternativamente a função pode ser definida da seguinte forma:__
@
jogadorbmp :: Int -> Picture
jogadorbmp n = | n > 3 = Translate 9.0 8.0 (Scale 0.7 0.7 (carregarBMP ("jogador" ++ show n ++ ".bmp"))
               | otherwise = Translate 9.0 8.0 (Scale 0.7 0.7 (carregarBMP ("jogador.bmp"))
@

== Exemplos de utilização:
>>> jogadorbmp 0
Translate 9.0 8.0 (Scale 0.7 0.7 (carregarBMP ("jogador.bmp"))

>>> jogadorbmp 1
Translate 9.0 8.0 (Scale 0.7 0.7 (carregarBMP ("jogador1.bmp"))

>>> jogadorbmp 2
Translate 9.0 8.0 (Scale 0.7 0.7 (carregarBMP ("jogador2.bmp"))

>>> jogadorbmp 3
Translate 9.0 8.0 (Scale 0.7 0.7 (carregarBMP ("jogador3.bmp"))
-}
jogadorbmp :: Int -- ^ Número do BMP do jogador (jogador selecionado - 0, 1, 2 ou 3)
                -> Picture -- ^ Retorna a imagem BMP do jogador em formato Picture
jogadorbmp 0 = Translate 9 8 (Scale 0.7 0.7 (carregarBMP "src/assets/jogador.bmp"))
jogadorbmp 1 = Translate 9 8 (Scale 0.7 0.7 (carregarBMP "src/assets/jogador1.bmp"))
jogadorbmp 2 = Translate 9 8 (Scale 0.7 0.7 (carregarBMP "src/assets/jogador2.bmp"))
jogadorbmp 3 = Translate 9 8 (Scale 0.7 0.7 (carregarBMP "src/assets/jogador3.bmp"))
jogadorbmp _ = Translate 9 8 (Scale 0.7 0.7 (carregarBMP "src/assets/jogador.bmp"))

{- |
Função 'seljogadorbmp' que retorna a 'Picture' do 'Menu' "Selecionar Jogador" dado o número deste ser selecionado (1) ou não (0).

== Exemplos de utilização:
>>> seljogadorbmp 0
carregarBMP ("seljogador.bmp")

>>> seljogadorbmp 1
carregarBMP ("seljogador_sel.bmp")
-}
seljogadorbmp :: Int -- ^ Número (0 ou 1) que define se o menu está selecionado (1) ou não (0)
                    -> Picture -- ^ Retorna a imagem BMP do menu em formato Picture
seljogadorbmp e | e == 1 = carregarBMP "src/assets/seljogador_sel.bmp"
                | otherwise = carregarBMP "src/assets/seljogador.bmp"

{- |
Função 'seljogadorselectorbmp' que retorna a 'Picture' do selector do menu "Selecionar Jogador" (Número do 'SubMenu') dado o número do jogador selecionado.

Caso o número do jogador selecionado não esteja definido, retorna a 'Picture' do selector do menu sem nenhum jogador selecionado (Seleciona a opção Sair do menu).

== Exemplos de utilização:
>>> seljogadorselectorbmp 0
carregarBMP ("seljogadorselector1.bmp")

>>> seljogadorselectorbmp 1
carregarBMP ("seljogadorselector2.bmp")

>>> seljogadorselectorbmp 2
carregarBMP ("seljogadorselector3.bmp")

>>> seljogadorselectorbmp 3
carregarBMP ("seljogadorselector4.bmp")

>>> seljogadorselectorbmp 4
carregarBMP ("seljogadorselector.bmp")
-}
seljogadorselectorbmp :: Int -- ^ Número do jogador selecionado (0, 1, 2 ou 3)
                            -> Picture -- ^ Retorna a imagem BMP do selector em formato Picture
seljogadorselectorbmp e | e == 0 = carregarBMP "src/assets/seljogadorselector1.bmp"
                        | e == 1 = carregarBMP "src/assets/seljogadorselector2.bmp"
                        | e == 2 = carregarBMP "src/assets/seljogadorselector3.bmp"
                        | e == 3 = carregarBMP "src/assets/seljogadorselector4.bmp"
                        | otherwise = carregarBMP "src/assets/seljogadorselector.bmp"

{- |
Função 'seldificuldadebmp' que retorna a 'Picture' do 'Menu' "Selecionar Dificuldade" dado o número deste ser selecionado (1) ou não (0).

== Exemplos de utilização:
>>> seldificuldadebmp 0
carregarBMP ("seldif.bmp")

>>> seldificuldadebmp 1
carregarBMP ("seldif_sel.bmp")
-}
seldificuldadebmp :: Int -- ^ Número (0 ou 1) que define se o menu está selecionado (1) ou não (0)
                        -> Picture -- ^ Retorna a imagem BMP do menu em formato Picture
seldificuldadebmp e | e == 1 = carregarBMP "src/assets/seldif_sel.bmp"
                    | otherwise = carregarBMP "src/assets/seldif.bmp"

{- |
Função 'seldificuldadeselectorbmp' que retorna a 'Picture' do selector do menu "Selecionar Dificuldade" (Número do 'SubMenu') dado o número da dificuldade selecionada.

Caso o número da dificuldade selecionada não esteja definido, retorna a 'Picture' do selector do menu sem nenhuma dificuldade selecionada (Seleciona a opção Sair do menu).

== Exemplos de utilização:
>>> seldificuldadeselectorbmp 0
carregarBMP ("seldifselector1.bmp")

>>> seldificuldadeselectorbmp 1
carregarBMP ("seldifselector2.bmp")

>>> seldificuldadeselectorbmp 2
carregarBMP ("seldifselector3.bmp")

>>> seldificuldadeselectorbmp 3
carregarBMP ("seldifselector4.bmp")

>>> seldificuldadeselectorbmp 4
carregarBMP ("seldifselector.bmp")
-}
seldificuldadeselectorbmp :: Int -> Picture
seldificuldadeselectorbmp e | e == 0 = carregarBMP "src/assets/seldifselector1.bmp"
                            | e == 1 = carregarBMP "src/assets/seldifselector2.bmp"
                            | e == 2 = carregarBMP "src/assets/seldifselector3.bmp"
                            | e == 3 = carregarBMP "src/assets/seldifselector4.bmp"
                            | otherwise = carregarBMP "src/assets/seldifselector.bmp"

-- | Função 'arvorebmp' que retorna a 'Picture' da 'Arvore'.
arvorebmp :: Picture
arvorebmp = carregarBMP "src/assets/arvore.bmp"

-- | Função 'troncobmp' que retorna a 'Picture' do 'Tronco'.
troncobmp :: Picture
troncobmp = carregarBMP "src/assets/tronco.bmp"

{- |
Função 'carrobmp' que retorna a 'Picture' do 'Carro' dado o um número que define a direção do carro.

Caso o número seja positivo, o carro vai para a direita, caso contrário, vai para a esquerda.

== Exemplos de utilização:
>>> carrobmp 1
carregarBMP ("carrod.bmp")

>>> carrobmp 2
carregarBMP ("carro1d.bmp")

>>> carrobmp (-1)
carregarBMP ("carroe.bmp")

>>> carrobmp (-2)
carregarBMP ("carro1e.bmp")

__Nota:__ O número serve também para escolher aleatoriamente entre as imagens dos carros para a direita ou para a esquerda, utilizando a função 'chooseRandom' do módulo "System.Random".
-}
carrobmp :: Int -- ^ Número que define a direção do carro (positivo para a direita, negativo para a esquerda)
            -> Picture -- ^ Retorna a imagem BMP do carro em formato Picture
carrobmp n | n >= 0 = chooseRandom n carrosd
           | otherwise = chooseRandom n carrose
           where carrod = carregarBMP "src/assets/carrod.bmp"
                 carro1d = carregarBMP "src/assets/carro1d.bmp"
                 carrosd = [carrod, carro1d]
                 carroe = carregarBMP "src/assets/carroe.bmp"
                 carro1e = carregarBMP "src/assets/carro1e.bmp"
                 carrose = [carroe, carro1e]

-- | Função 'logobmp' que retorna a 'Picture' do logo do jogo no 'Menu'.
logobmp :: Picture
logobmp = carregarBMP "src/assets/crossytoad.bmp"

-- | Função 'instrucoespainelbmp' que retorna a 'Picture' do painel de instruções no 'SubMenu' instruções.
instrucoespainelbmp :: Picture
instrucoespainelbmp = carregarBMP "src/assets/instrucoespainel.bmp"

-- | Função 'bgbmp' que retorna a 'Picture' do fundo do jogo.
bgbmp :: Picture
bgbmp = Scale ((fromIntegral (fst initScreenSize))/580) ((fromIntegral (snd initScreenSize))/580) (carregarBMP "src/assets/bg.bmp")

{- |
Função 'menujogar' que retorna a 'Picture' do 'Menu' Jogar dado o número deste ser selecionado (1) ou não (0).

== Exemplos de utilização:
>>> menujogar 1
carregarBMP ("jogar_sel.bmp")

>>> menujogar 0
carregarBMP ("jogar.bmp")
-}
menujogar :: Int -- ^ Número que define se o menu Jogar está selecionado ou não (1 para selecionado, 0 para não selecionado)
                -> Picture -- ^ Retorna a imagem BMP do menu Jogar em formato Picture
menujogar e | e == 1 = carregarBMP "src/assets/jogar_sel.bmp"
            | otherwise = carregarBMP "src/assets/jogar.bmp"

{- |
Função 'menusair' que retorna a 'Picture' do 'Menu' Sair dado o número deste ser selecionado (1) ou não (0).

== Exemplos de utilização:
>>> menusair 1
carregarBMP ("sair_sel.bmp")

>>> menusair 0
carregarBMP ("sair.bmp")
-}
menusair :: Int -- ^ Número que define se o menu Sair está selecionado ou não (1 para selecionado, 0 para não selecionado)
                -> Picture -- ^ Retorna a imagem BMP do menu Sair em formato Picture
menusair e | e == 1 = carregarBMP "src/assets/sair_sel.bmp"
           | otherwise = carregarBMP "src/assets/sair.bmp"

{- |
Função 'menuinstrucoes' que retorna a 'Picture' do 'Menu' Instruções dado o número deste ser selecionado (1) ou não (0).

== Exemplos de utilização:
>>> menuinstrucoes 1
carregarBMP ("instrucoes_sel.bmp")

>>> menuinstrucoes 0
carregarBMP ("instrucoes.bmp")
-}
menuinstrucoes :: Int -- ^ Número que define se o menu Instruções está selecionado ou não (1 para selecionado, 0 para não selecionado)
                    -> Picture -- ^ Retorna a imagem BMP do menu Instruções em formato Picture
menuinstrucoes e | e == 1 = carregarBMP "src/assets/instrucoes_sel.bmp"
                 | otherwise = carregarBMP "src/assets/instrucoes.bmp"

-- | Função 'menugameover' que retorna a 'Picture' do 'Menu' Game Over quando o jogador perde.
menugameover :: Picture
menugameover = carregarBMP "src/assets/gameover.bmp"

-- | Função 'tamanhobmp' que retorna o tamanho de cada BMP do jogo.
tamanhobmp :: Int
tamanhobmp = 64

{- |
Função 'listaLimpa' que gera uma lista de ['Obstaculo'] apenas com 'Nenhum', com o tamanho dado.

== Exemplos de utilização:
>>> listaLimpa 3
[Nenhum, Nenhum, Nenhum]

>>> listaLimpa 5
[Nenhum, Nenhum, Nenhum, Nenhum, Nenhum]

== Propriedades:
prop> listaLimpa 0 == []
-}
listaLimpa :: Int -> [Obstaculo]
listaLimpa 0 = []
listaLimpa n = [Nenhum] ++ listaLimpa (n-1)

{- |
Função 'geraMapa' que gera um 'Mapa' aleatório com um tamanho aleatório entre 6 e o tamanho máximo possível de ecrã.

__Nota:__ Esta função utiliza a função 'geraMapaAux', bem como a função 'mapaValido' das tarefas anteriores para verificar que o mapa gerado é válido.
-}
geraMapa :: IO Mapa
geraMapa = do
    w <- randomRIO (6, div (fst initScreenSize) tamanhobmp)
    h <- randomRIO (6, div (snd initScreenSize) (tamanhobmp+w))
    mapa <- geraMapaAux (Mapa w [(Relva, listaLimpa w)]) w
    if mapaValido mapa then return mapa else geraMapa

{- |
Função 'geraMapaAux' que estende um 'Mapa' aleatório, n @::Int@ vezes, utilizando a função 'estendeMapa' de tarefas anteriores, gerando-se um 'Mapa'.

__Nota:__ Esta função utiliza um número aleatório entre 0 e 100 para a função 'estendeMapa' como pedido no enunciado da Tarea 2.

== Propriedades:
prop> geraMapaAux mapa 0 == mapa
-}
geraMapaAux :: Mapa -- ^ Mapa a estender
                -> Int -- ^ Número de vezes que o mapa é estendido
                -> IO Mapa -- ^ Retorna o mapa estendido
geraMapaAux mapa 0 = return mapa
geraMapaAux mapa@(Mapa w l) n = do
    h <- randomRIO (0, 100) -- número aleatório entre 0 e 100 para a função @estendeMapa@ como pedido no enunciado da Tarea 2
    mapa <- geraMapaAux (estendeMapa mapa h) (n-1)
    return mapa

-- lê o "top score" do ficheiro "savegame.txt"
-- | Função 'getSavegame' que lê os dados salvos no ficheiro "savegame.txt" e retorna-os em forma de tuplo ('Score', 'Difficulty', 'JogadorBMP').
getSavegame :: IO (Int, Int, Int) -- ^ Retorna o 'Score', 'Difficulty' e 'JogadorBMP' lidos
getSavegame = do
    savegame <- readFile "savegame.txt"
    let [score, dif, jogadorbmp] = map read (lines savegame)
    return (score, dif, jogadorbmp)

-- | Função 'writeSavegame' que escreve os 'Score', 'Difficulty' e 'JogadorBMP' dados e salva-os no ficheiro "savegame.txt" e retorna-os em forma de tuplo ('Score', 'Difficulty', 'JogadorBMP').
writeSavegame :: Int -- ^ 'Score' a ser escrito
                -> Int -- ^ 'Difficulty' a ser escrita
                -> Int -- ^ 'JogadorBMP' a ser escrita
                -> IO (Int, Int, Int) -- ^ Retorna o 'Score', 'Difficulty' e 'JogadorBMP' escritos
writeSavegame score dif jogadorbmp = do
    writeFile "savegame.txt" (show score ++ "\n" ++ show dif ++ "\n" ++ show jogadorbmp)
    return (score, dif, jogadorbmp)

-- | Função 'estadoInicial' que retorna o 'Estado' inicial do jogo.
estadoInicial :: Int -- ^ Top 'Score' inicial lido
                -> Int -- ^ 'Difficulty' inicial lido
                -> Int -- ^ 'JogadorBMP' inicial lido
                -> Mapa -- ^ 'Mapa' inicial gerado
                -> Estado -- ^ Retorna o 'Estado' inicial do jogo
estadoInicial topScore dif djogador mapa = ((True, 0, 0, 0, topScore, 0, dif, djogador), Jogo (Jogador (0,0)) mapa)

-- | Função 'desenhaMenu' que desenha o 'Estado' do jogo.
desenha :: Estado -> IO Picture
desenha ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), (Jogo jogador@(Jogador (x,y)) mapa@(Mapa l lista@((terreno, obstaculos):t)))) | menu && menuPos >= 0 = return $ Pictures [bgbmp, Translate (-m1) (m2-64) (Pictures (desenhaMenu menuPos submenuPos)), Translate (fromIntegral (div (fst initScreenSize) 6)) (-((fromIntegral (div (snd initScreenSize) 3))+120)) (Scale 0.5 0.5 (Text ("Top Score:"++(show topScore)))), Translate (-m1) (m2+218) logobmp]
                                                                                                                      | menu && menuPos == (-1) = return $ Pictures [bgbmp, Translate (-m1) (m2+128) (Pictures (desenhaMenu menuPos submenuPos)), Translate (fromIntegral (div (fst initScreenSize) 6)) (-((fromIntegral (div (snd initScreenSize) 3))+120)) (Scale 0.5 0.5 (Text ("Top Score:"++(show topScore))))]
                                                                                                                      | menu && menuPos == (-2) = return $ Pictures [bgbmp, Translate (-m1) (m2+128) (Pictures (desenhaMenu menuPos submenuPos))]
                                                                                                                      | menu && menuPos == (-3) = return $ Pictures [bgbmp, Translate (-m1) (m2+128) (Pictures (desenhaMenu menuPos submenuPos))]
                                                                                                                      | menu && menuPos == (-4) = return $ Pictures [bgbmp, Translate (-m1) (m2+128) (Pictures (desenhaMenu menuPos submenuPos))]
                                                                                                                      | otherwise = return $ Pictures [bgbmp, Translate (-p1) (-p2) (Pictures [desenharMapa mapa 0 0, desenhaJogador jogador djogador]), Translate (fromIntegral (div (fst initScreenSize) 5)) (-((fromIntegral (div (snd initScreenSize) 3))+120)) (Scale 0.5 0.5 (Text ("Score:"++(show score))))]
                                                                                                                      where p1 = fromIntegral (l*tamanhobmp) / 2
                                                                                                                            p2 = fromIntegral (length lista*tamanhobmp) / 2
                                                                                                                            m1 = fromIntegral (div (fst initScreenSize) tamanhobmp)
                                                                                                                            m2 = fromIntegral (div (snd initScreenSize) tamanhobmp)

-- | Função 'desenhaMenu' que desenha o menu do jogo dado o seu 'Estado' de 'Menu' e 'SubMenu'.
desenhaMenu :: Int -> Int -> [Picture]
-- Menu
desenhaMenu 0 _ = [bgbmp, Translate 0 128 (menujogar 1), Translate 0 0 (menuinstrucoes 0), Translate 0 (-128) (seljogadorbmp 0), Translate 0 (-(128*2)) (seldificuldadebmp 0), Translate 0 (-(128*3)) (menusair 0)]
desenhaMenu 1 _ = [bgbmp, Translate 0 128 (menujogar 0), Translate 0 0 (menuinstrucoes 1), Translate 0 (-128) (seljogadorbmp 0), Translate 0 (-(128*2)) (seldificuldadebmp 0), Translate 0 (-(128*3)) (menusair 0)]
desenhaMenu 2 _ = [bgbmp, Translate 0 128 (menujogar 0), Translate 0 0 (menuinstrucoes 0), Translate 0 (-128) (seljogadorbmp 1), Translate 0 (-(128*2)) (seldificuldadebmp 0), Translate 0 (-(128*3)) (menusair 0)]
desenhaMenu 3 _ = [bgbmp, Translate 0 128 (menujogar 0), Translate 0 0 (menuinstrucoes 0), Translate 0 (-128) (seljogadorbmp 0), Translate 0 (-(128*2)) (seldificuldadebmp 1), Translate 0 (-(128*3)) (menusair 0)]
desenhaMenu 4 _ = [bgbmp, Translate 0 128 (menujogar 0), Translate 0 0 (menuinstrucoes 0), Translate 0 (-128) (seljogadorbmp 0), Translate 0 (-(128*2)) (seldificuldadebmp 0), Translate 0 (-(128*3)) (menusair 1)]
-- Sub-menus
desenhaMenu (-1) _ = [bgbmp, Translate 0 0 menugameover, Translate 0 (-128) (menusair 1)] -- Death screen
desenhaMenu (-2) _ = [bgbmp, Translate 0 74 (menuinstrucoes 0), Translate 32 (-228) instrucoespainelbmp, Translate 0 (-528) (menusair 1)] -- Instructions screen
desenhaMenu (-3) b | b == 0 = [bgbmp, Translate 0 (-64) (jogadorbmp b), Translate 0 0 (seljogadorbmp 0), Translate 0 (-248) (seljogadorselectorbmp b), Translate 0 (-528) (menusair 0)] -- Select player screen
                   | b == 1 = [bgbmp, Translate 0 (-64) (jogadorbmp b), Translate 0 0 (seljogadorbmp 0), Translate 0 (-248) (seljogadorselectorbmp b), Translate 0 (-528) (menusair 0)]
                   | b == 2 = [bgbmp, Translate 0 (-64) (jogadorbmp b), Translate 0 0 (seljogadorbmp 0), Translate 0 (-248) (seljogadorselectorbmp b), Translate 0 (-528) (menusair 0)]
                   | b == 3 = [bgbmp, Translate 0 (-64) (jogadorbmp b), Translate 0 0 (seljogadorbmp 0), Translate 0 (-248) (seljogadorselectorbmp b), Translate 0 (-528) (menusair 0)]
                   | otherwise = [bgbmp, Translate 0 0 (seljogadorbmp 0), Translate 0 (-248) (seljogadorselectorbmp 5), Translate 0 (-528) (menusair 1)]
desenhaMenu (-4) b | b == 0 = [bgbmp, Translate 0 0 (seldificuldadebmp 0), Translate 0 (-248) (seldificuldadeselectorbmp b), Translate 0 (-528) (menusair 0)]
                   | b == 1 = [bgbmp, Translate 0 0 (seldificuldadebmp 0), Translate 0 (-248) (seldificuldadeselectorbmp b), Translate 0 (-528) (menusair 0)]
                   | b == 2 = [bgbmp, Translate 0 0 (seldificuldadebmp 0), Translate 0 (-248) (seldificuldadeselectorbmp b), Translate 0 (-528) (menusair 0)]
                   | b == 3 = [bgbmp, Translate 0 0 (seldificuldadebmp 0), Translate 0 (-248) (seldificuldadeselectorbmp b), Translate 0 (-528) (menusair 0)]
                   | otherwise = [bgbmp, Translate 0 0 (seldificuldadebmp 0), Translate 0 (-248) (seldificuldadeselectorbmp 5), Translate 0 (-528) (menusair 1)]
desenhaMenu n b | n >= 0 = desenhaMenu (mod n 5) b
                | otherwise = desenhaMenu (abs n) b

-- | Função 'desenhaJogo' que desenha o jogo dada a sua posição e 'JogadorBPM' selecionado.
desenhaJogador :: Jogador -- ^ Estado e posição do jogador
                    -> Int -- ^ JogadorBPM selecionado
                    -> Picture -- ^ Imagem do jogador
desenhaJogador (Jogador (x,y)) n = Translate p1 p2 (jogadorbmp n)
                                where (p1, p2) = (fromIntegral (x*tamanhobmp), fromIntegral (y*tamanhobmp))

-- Função que desenha as linhas de um mapa (ex: se o comprimento "l" for 3, desenha 3 quadrados na horizontal)
-- | Função 'desenhaLinha' que desenha o mapa do jogo.
desenharMapa :: Mapa -- ^ Mapa do jogo
                -> Int -- ^ Posição x utilizada para desenhar os obstaculos e linhas do mapa (incremental)
                -> Int -- ^ Posição y utilizada para desenhar os obstaculos e linhas do mapa (incremental)
                -> Picture -- ^ Imagem do mapa
desenharMapa (Mapa _ []) x y = Translate p1 p2 Blank
                                where (p1, p2) = (fromIntegral x, fromIntegral y)
desenharMapa (Mapa l ((Relva, obstaculos):t)) x y = Pictures [desenhaLinha l p1 p2 relvabmp, desenhaObstaculos obstaculos p1 p2 0, desenharMapa (Mapa l t) x (y+tamanhobmp)]
                                                where (p1, p2) = (fromIntegral x, fromIntegral y)
desenharMapa (Mapa l ((Rio velocidade, obstaculos):t)) x y = Pictures [desenhaLinha l p1 p2 aguabmp, desenhaObstaculos obstaculos p1 p2 velocidade, desenharMapa (Mapa l t) x (y+tamanhobmp)]
                                                where (p1, p2) = (fromIntegral x, fromIntegral y)
desenharMapa (Mapa l ((Estrada velocidade, obstaculos):t)) x y = Pictures [desenhaLinha l p1 p2 estradabmp, desenhaObstaculos obstaculos p1 p2 velocidade, desenharMapa (Mapa l t) x (y+tamanhobmp)]
                                                where (p1, p2) = (fromIntegral x, fromIntegral y)

-- | Função 'desenhaLinha' que desenha uma linha do mapa do jogo.
desenhaLinha :: Int -- ^ Comprimento da linha / Largura do mapa
                -> Int -- ^ Posição x utilizada para desenhar a linha
                -> Int -- ^ Posição y utilizada para desenhar a linha
                -> Picture -- ^ Imagem do BMP a utilizar como fundo da linha ('Relva', 'Rio', 'Estrada')
                -> Picture -- ^ Imagem da linha do mapa com o comprimento dado
desenhaLinha 0 x y bmp = Translate p1 p2 Blank
                        where (p1, p2) = (fromIntegral x, fromIntegral y)
desenhaLinha l x y bmp = Pictures [Translate p1 p2 bmp, desenhaLinha (l-1) (x+tamanhobmp) y bmp]
                        where (p1, p2) = (fromIntegral x, fromIntegral y)

-- Função que desenha os obstáculos de uma linha
-- | Função 'desenhaObstaculos' que desenha os obstáculos de uma linha do mapa do jogo.
desenhaObstaculos :: [Obstaculo] -- ^ Lista de obstáculos de uma linha do mapa
                        -> Int -- ^ Posição x utilizada para desenhar os obstaculos
                        -> Int -- ^ Posição y utilizada para desenhar os obstaculos
                        -> Int -- ^ Velocidade do 'Rio' ou 'Estrada'
                        -> Picture -- ^ Imagem dos obstáculos da linha do mapa
desenhaObstaculos [] x y v = Translate p1 p2 Blank
                        where (p1, p2) = (fromIntegral x, fromIntegral y)
desenhaObstaculos (Arvore:t) x y v = Pictures [Translate p1 p2 arvorebmp, desenhaObstaculos t (x+tamanhobmp) y v]
                        where (p1, p2) = (fromIntegral x, fromIntegral y)
desenhaObstaculos (Tronco:t) x y v = Pictures [Translate p1 p2 troncobmp, desenhaObstaculos t (x+tamanhobmp) y v]
                        where (p1, p2) = (fromIntegral x, fromIntegral y)
desenhaObstaculos (Carro:t) x y v = Pictures [Translate p1 p2 (carrobmp v), desenhaObstaculos t (x+tamanhobmp) y v]
                        where (p1, p2) = (fromIntegral x, fromIntegral y)
desenhaObstaculos (Nenhum:t) x y v = Pictures [Translate p1 p2 Blank, desenhaObstaculos t (x+tamanhobmp) y v]
                        where (p1, p2) = (fromIntegral x, fromIntegral y)

-- | Função 'evento' que reage quando o utilizador carrega numa tecla.
evento :: Event -> Estado -> IO Estado
evento (EventKey (SpecialKey KeyUp) Down _ _) ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) (Mapa l mapa))) | menu && menuPos > 0 = return $ ((menu,menuPos-1,submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                         | menu && menuPos == 0 = return $ ((menu,(mod (menuPos-1) 5),submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                         | menu && menuPos < 0 = return $ ((menu,menuPos,(mod (submenuPos-1) 5),score,topScore,dtime,dif,djogador), jogo)
                                                                                                                         | otherwise = if (animaJogador jogo (Move Cima)) == (Jogador (x,y)) then return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                                       else return $ ((menu,menuPos,submenuPos,score+1,topScore,dtime,dif,djogador), (Jogo (animaJogador jogo (Move Cima)) (Mapa l mapa)))
evento (EventKey (SpecialKey KeyDown) Down _ _) ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) (Mapa l mapa))) | menu && menuPos >= 0 = return $ ((menu,menuPos+1,submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos < 0 = return $ ((menu,menuPos,(mod (submenuPos+1) 5),score,topScore,dtime,dif,djogador), jogo)
                                                                                                                           | y == 0  = return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), (Jogo (animaJogador jogo (Move Baixo)) (Mapa l mapa)))
                                                                                                                           | otherwise = return $ ((menu,menuPos,submenuPos,score-1,topScore,dtime,dif,djogador), (Jogo (animaJogador jogo (Move Baixo)) (Mapa l mapa)))
evento (EventKey (SpecialKey KeyLeft) Down _ _) ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) (Mapa l mapa))) | menu = return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                           | otherwise = return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), (Jogo (animaJogador jogo (Move Esquerda)) (Mapa l mapa)))
evento (EventKey (SpecialKey KeyRight) Down _ _) ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) (Mapa l mapa))) | menu = return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | otherwise = return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), (Jogo (animaJogador jogo (Move Direita)) (Mapa l mapa)))
evento (EventKey (SpecialKey KeyEnter) Down _ _) ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) (Mapa l mapa))) | menu && menuPos >= 0 && (mod menuPos 5) == 0 = return $ ((False,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos >= 0 && (mod menuPos 5) == 1 = return $ ((True,(-2),submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos >= 0 && (mod menuPos 5) == 2 = return $ ((True,(-3),submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos >= 0 && (mod menuPos 5) == 3 = return $ ((True,(-4),submenuPos,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos >= 0 && (mod menuPos 5) == 4 = do writeSavegame topScore dif djogador
                                                                                                                                                                                exitSuccess
                                                                                                                            | menu && menuPos < 0 && (mod menuPos 4) == 3 = return $ ((True,0,0,score,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos < 0 && (mod menuPos 4) == 2 = return $ ((True,0,0,0,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos < 0 && (mod menuPos 4) == 1 = if (mod (abs submenuPos) 5) <= 3 then
                                                                                                                                                                            do
                                                                                                                                                                                let newJogadorbmp = (mod (abs submenuPos) 5)
                                                                                                                                                                                return $ ((True,0,0,newJogadorbmp,topScore,dtime,dif,newJogadorbmp), jogo)
                                                                                                                                                                            else
                                                                                                                                                                                return $ ((True,0,0,0,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | menu && menuPos < 0 && (mod menuPos 4) == 0 = if (mod (abs submenuPos) 5) <= 3 then
                                                                                                                                                                            do
                                                                                                                                                                                let newDif = (mod (abs submenuPos) 5)
                                                                                                                                                                                return $ ((True,0,0,newDif,topScore,dtime,newDif,djogador), jogo)
                                                                                                                                                                            else
                                                                                                                                                                                return $ ((True,0,0,0,topScore,dtime,dif,djogador), jogo)
                                                                                                                            | otherwise = return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo)
evento (EventKey (SpecialKey KeyEsc) Down _ _) ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) (Mapa l mapa))) = do
                                                                                                                            novoMapa <- geraMapa
                                                                                                                            return $ ((True,0,submenuPos,0,topScore,dtime,dif,djogador), (Jogo (Jogador (0,0)) novoMapa))
evento _ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) (Mapa l mapa))) = return $ ((menu,menuPos,submenuPos,score,topScore,dtime,dif,djogador), (Jogo (animaJogador jogo (Parado)) (Mapa l mapa)))
--evento _ e = e -- Todas as teclas (caso de paragem, previne erros, em cima colocamos as teclas que pretendemos utilizar)

-- | Função 'tempo' que atualiza o estado do jogo em função do tempo decorrido.
tempo :: Float -> Estado -> IO Estado
tempo time e@(menu@(menuStat,menuPos,submenuPos,score,topScore,dtime,dif,djogador), jogo@(Jogo (Jogador (x,y)) mapa@(Mapa l linhas))) | jogoTerminou jogo && score > topScore = do
                                                                                                                                                writeSavegame score dif djogador
                                                                                                                                                novoMapa <- geraMapa
                                                                                                                                                return $ ((True, (-1), submenuPos, 0, score, dtime,dif,djogador), (Jogo (Jogador (0,0)) novoMapa))
                                                                                                     | jogoTerminou jogo = do
                                                                                                                            writeSavegame topScore dif djogador
                                                                                                                            novoMapa <- geraMapa
                                                                                                                            return $ ((True, (-1), submenuPos, 0, topScore, dtime,dif,djogador), (Jogo (Jogador (0,0)) novoMapa))
                                                                                                     | not menuStat && deltaTime && score /= 0 && dif >= 2 = return $ ((menuStat,menuPos,submenuPos,score,topScore,dtime+time,dif,djogador), animaJogo (deslizaJogo (round dtime) jogo) (Parado))
                                                                                                     | menuStat = return $ e
                                                                                                     | otherwise = return $ ((menuStat,menuPos,submenuPos,score,topScore,dtime+time,dif,djogador), animaJogo (animaDeslizaJogo (round dtime) jogo 4) (Parado))
                                                                                                     where deltaTime = mod (round dtime) 3 <= 1

-- | Função 'frameRate' que define o número de frames por segundo dada uma dificuldade.
frameRate :: Int -- ^ 'Difficulty' dada
                -> Int -- ^ Framerate a retornar
frameRate 0 = 1 -- executa as funções "tempo" e "evento" 1 vezes por segundo
frameRate 1 = 2
frameRate 2 = 4
frameRate 3 = 8

-- | Função 'janela' que define o formato, tamanho, titulo e posição, da janela do jogo.
janela :: Display
janela = InWindow "Crossy Toad" initScreenSize (0,0)

-- returns the screen size in touple (width, height), using "getScreenSize" from Graphics.Gloss.Interface.Environment
-- | Função 'initScreenSize' que retorna o tamanho do ecrã.
initScreenSize :: (Int, Int) -- ^ Tuplo do tamanho do ecrã (largura, altura)
initScreenSize = unsafePerformIO getScreenSize

-- | Função 'main' que inicia e corre o jogo utilizando a função 'playIO' do Gloss.
main :: IO ()
main = do
    mapa <- geraMapa
    (topScore, dif, djogador) <- getSavegame
    playIO janela white (frameRate dif) (estadoInicial topScore dif djogador mapa) desenha evento tempo