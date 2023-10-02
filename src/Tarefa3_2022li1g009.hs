{- |
Module      : Tarefa3_2022li1g009
Description : Movimentação do personagem e obstáculos
Copyright   : Fábio Magalhães <a104365@alunos.uminho.pt>
              Pedro Rosário <a100536@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.

__Nota:__ @Para a realização desta tarefa, foi necessário importar o tipo @ 'Linha' @ da Tarefa 1, que se encontra no módulo 'Tarefa1_2022li1g009'.@
-}
module Tarefa3_2022li1g009 (
    -- * Funções principais
    -- $principal
    animaJogo, animaJogador, animaEstradaRio,
    -- * Funções auxiliares
    -- $aux
    rotateListRight, rotateListLeft, moveObstaculos, naoMoveForaMapa, naoMoveCimaArvore, paradoEmCimaTronco, paradoEmCimaTroncoAux, velocidadeTronco
    ) where

{- $principal
Conjunto de funções que são executadas para animar o jogo.
-}

{- $aux
Conjunto de funções auxiliares que realizam tarefas específicas para restrigir o movimento do jogador e dos obstáculos.
-}

import LI12223

import Tarefa1_2022li1g009 as T1

{- |
A função 'animaJogo' que anima o jogo, movendo o jogador, dado um movimento, e os obstáculos.

== Exemplos de utilização:
>>> animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) (Move Cima)
Jogo (Jogador (1,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum])])

>>> animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])) (Parado)
Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Carro])])
-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo jogo@(Jogo jogador@(Jogador (x,y)) mapa@(Mapa l linhas)) jogada = (Jogo novasCoordenadas (Mapa l novasLinhas))
    where novasCoordenadas = animaJogador jogo jogada
          novasLinhas = animaEstradaRio linhas

-- 3.1.1
{- |
A função 'animaEstradaRio' que anima a estrada e o rio, movendo os obstáculos.

Função que numa estrada ou rio com velocidade @v@ os obstáculos devem mover-se @|v|@ unidades na direção determinada.

== Exemplos de utilização:
>>> animaEstradaRio [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]
[(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum])]

== Propriedades:
prop> animaEstradaRio [] = []
-}
animaEstradaRio :: [T1.Linha] -> [T1.Linha]
animaEstradaRio [] = []
animaEstradaRio ((Rio velocidade, obstaculos):t) = (Rio velocidade, moveObstaculos velocidade obstaculos):animaEstradaRio t
animaEstradaRio ((Estrada velocidade, obstaculos):t) = (Estrada velocidade, moveObstaculos velocidade obstaculos):animaEstradaRio t
animaEstradaRio ((Relva, obstaculos):t) = (Relva, obstaculos):animaEstradaRio t

{- |
A função 'rotateListRight' auxiliar que roda todos os obstáculos de uma lista 'n' vezes para a direita.

== Exemplos de utilização:
>>> rotateListRight 1 [1, 2, 3]
[[1,2,3],[3,1,2]]

>>> rotateListRight 2 [1, 2, 3]
[[1,2,3],[3,1,2],[2,3,1]]

== Propriedades:
prop> rotateListRight 0 [l] = [l]
-}
rotateListRight :: Int -- ^ Número de vezes que a lista deve ser rotacionada @n@
              -> [a] -- ^ Lista de elementos
              -> [[a]] -- ^ Lista de listas de elementos originada da rotação da lista original por cada passo @n@ vezes
rotateListRight n [] = error "Lista vazia"
rotateListRight 0 l = [l]
rotateListRight n l = l : rotateListRight (n-1) ((last l):(init l))

{- |
A função 'rotateListLeft' auxiliar que roda todos os obstáculos de uma lista 'n' vezes para a esquerda.

== Exemplos de utilização:
>>> rotateListLeft 1 [1, 2, 3]
[[1,2,3],[2,3,1]]

>>> rotateListLeft 2 [1, 2, 3]
[[1,2,3],[2,3,1],[3,1,2]]

== Propriedades:
prop> rotateListLeft 0 [l] = [l]
-}
rotateListLeft :: Int -- ^ Número de vezes que a lista deve ser rotacionada @n@
              -> [a] -- ^ Lista de elementos
              -> [[a]] -- ^ Lista de listas de elementos originada da rotação da lista original por cada passo @n@ vezes
rotateListLeft n [] = error "Lista vazia"
rotateListLeft 0 l = [l]
rotateListLeft n l = l : rotateListLeft (n-1) ((tail l) ++ [head l])

-- 3.1.5
{- |
A função 'moveObstaculos' que move os obstáculos para a direita 'n' vezes ou para a esquerda 'n' vezes.

== Exemplos de utilização:
>>> moveObstaculos 1 [Carro, Nenhum, Nenhum]
[Nenhum,Carro,Nenhum]

>>> moveObstaculos 2 [Carro, Nenhum, Nenhum]
[Nenhum,Nenhum,Carro]

== Propriedades:
prop> moveObstaculos _ [] = []
prop> moveObstaculos 0 [l] = [l]
-}
moveObstaculos :: Int -> [Obstaculo] -> [Obstaculo]
moveObstaculos _ [] = []
moveObstaculos 0 obstaculos = obstaculos
moveObstaculos n obstaculos | n > 0 = last (rotateListRight n obstaculos) -- move para a direita
                            | otherwise = last (rotateListLeft (abs n) obstaculos) -- move para a esquerda

-- 3.1.2
{- |
A função 'animaJogador' que move o jogador segundo a jogada.

== Exemplos de utilização:
>>> animaJogador (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) (Move Cima)
Jogador (1,2)

>>> animaJogador (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])) (Parado)
Jogador (1,1)

>>> animaJogador (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])) (Parado)
Jogador (1,0)

== Propriedades:
__ Caso o jogador não se mova, a função deve devolver as mesmas coordenadas do jogador, ou caso esteja parado sobre um tronco move-se segundo este utilizando a função 'paradoEmCimaTronco'. __
-}
animaJogador :: Jogo -- ^ Jogo inicial (estado anterior ao movimento do jogador)
                -> Jogada -- ^ Jogada a ser realizada
                -> Jogador -- ^ Jogador após a jogada
animaJogador (Jogo jogador@(Jogador (x,y)) mapa@(Mapa l linhas)) (Parado) = paradoEmCimaTronco jogador mapa
animaJogador (Jogo jogador@(Jogador (x,y)) mapa@(Mapa l linhas)) (Move d) = case d of Cima -> if naoMoveForaMapa (Jogador (x,y+1)) mapa && naoMoveCimaArvore (Jogador (x,y+1)) mapa then Jogador (x,y+1) else jogador
                                                                                      Baixo -> if naoMoveForaMapa (Jogador (x,y-1)) mapa && naoMoveCimaArvore (Jogador (x,y-1)) mapa then Jogador (x,y-1) else jogador
                                                                                      Esquerda -> if naoMoveForaMapa (Jogador (x-1,y)) mapa && naoMoveCimaArvore (Jogador (x-1,y)) mapa then Jogador (x-1,y) else jogador
                                                                                      Direita -> if naoMoveForaMapa (Jogador (x+1,y)) mapa && naoMoveCimaArvore (Jogador (x+1,y)) mapa then Jogador (x+1,y) else jogador

{- |
Função auxiliar 'naoMoveCimaArvore' que verifica se o jogador não se move para cima de uma árvore.

== Exemplos de utilização:
>>> naoMoveCimaArvore (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])
True

>>> naoMoveCimaArvore (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]), (Relva, [Nenhum, Arvore, Nenhum])]) -- False
False
-}
naoMoveCimaArvore :: Jogador -> Mapa -> Bool
naoMoveCimaArvore (Jogador (x,y)) (Mapa l []) = True
naoMoveCimaArvore (Jogador (x,y)) (Mapa l ((Relva, obstaculos):t)) | y == 0 && obstaculos !! x == Arvore = False
                                                                   | otherwise = naoMoveCimaArvore (Jogador (x,y-1)) (Mapa l t)
naoMoveCimaArvore (Jogador (x,y)) (Mapa l ((Estrada v, obstaculos):t)) = naoMoveCimaArvore (Jogador (x,y-1)) (Mapa l t)
naoMoveCimaArvore (Jogador (x,y)) (Mapa l ((Rio v, obstaculos):t)) = naoMoveCimaArvore (Jogador (x,y-1)) (Mapa l t)

-- 3.1.4
{- |
A função 'naoMoveForaMapa' que verifica se o jogador não se move para uma posição fora do mapa.

Retorna 'True' se o jogador não se move para fora do mapa, 'False' caso contrário.

== Exemplos de utilização:
>>> naoMoveForaMapa (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])
True

>>> naoMoveForaMapa (Jogador (1,2)) (Mapa 2 [(Rio 1, [Tronco, Nenhum]), (Estrada 2, [Carro, Nenhum])])
False
-}
naoMoveForaMapa :: Jogador -> Mapa -> Bool
naoMoveForaMapa (Jogador (x,y)) (Mapa l linhas) | x < 0 || x >= l || y < 0 || y >= (length linhas) = False
                                                | otherwise = True

-- 3.1.3
{- |
A função 'paradoEmCimaTronco' que retorna a posição de um jogador que está parado em cima de um tronco em movimento e move-o.
Isto é, caso o jogador esteja parado em cima de um tronco, a função deve devolver as mesmas coordenadas do jogador, ou caso esteja parado sobre um tronco move-se segundo este.

== Exemplos de utilização:
>>> paradoEmCimaTronco (Jogador (0,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])
Jogador (1,0)

>>> paradoEmCimaTronco (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])
Jogador (1,1)
-}
paradoEmCimaTronco :: Jogador -> Mapa -> Jogador
paradoEmCimaTronco jogador@(Jogador (x,y)) mapa@(Mapa l linhas) | paradoEmCimaTroncoAux jogador mapa && velocidade /= 0 && velocidade > 0 = (Jogador (x+(mod (abs velocidade) l),y))
                                                                | paradoEmCimaTroncoAux jogador mapa && velocidade /= 0 && velocidade < 0 = (Jogador (x-(mod (abs velocidade) l),y))
                                                                | otherwise = jogador
                                                                where velocidade = velocidadeTronco jogador mapa

{- |
A função 'velocidadeTronco' auxiliar que devolve a direção do tronco em que o jogador está parado.

== Exemplos de utilização:
>>> velocidadeTronco (Jogador (0,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])
1

== Propriedades:
prop> velocidadeTronco (Jogador (x,y)) (Mapa l []) = 0

__ Caso o jogador não esteja parado em cima de um tronco, a função deve devolver 0. __
-}
velocidadeTronco :: Jogador -> Mapa -> Int
velocidadeTronco (Jogador (x,y)) (Mapa l []) = 0
velocidadeTronco (Jogador (x,y)) (Mapa l ((Estrada velocidade, obstaculos):t)) = velocidadeTronco (Jogador (x,y-1)) (Mapa l t)
velocidadeTronco (Jogador (x,y)) (Mapa l ((Relva, obstaculos):t)) = velocidadeTronco (Jogador (x,y-1)) (Mapa l t)
velocidadeTronco (Jogador (x,y)) (Mapa l ((Rio velocidade, obstaculos):t)) | y /= 0 = velocidadeTronco (Jogador (x,y-1)) (Mapa l t)
                                                                           | otherwise = velocidade

{- |
A função 'paradoEmCimaTroncoAux' auxiliar que verifica se um jogador está parado em cima de um tronco.

Retorna 'True' se o jogador está parado em cima de um tronco, 'False' caso contrário.

== Exemplos de utilização:
>>> paradoEmCimaTroncoAux (Jogador (0,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])
True

>>> paradoEmCimaTroncoAux (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])
False

== Propriedades:
prop> paradoEmCimaTroncoAux (Jogador (x,y)) (Mapa l []) = False
-}
paradoEmCimaTroncoAux :: Jogador -> Mapa -> Bool
paradoEmCimaTroncoAux (Jogador (x,y)) (Mapa l []) = False
paradoEmCimaTroncoAux (Jogador (x,y)) (Mapa l ((Estrada velocidade, obstaculos):t)) = False || paradoEmCimaTroncoAux (Jogador (x,y-1)) (Mapa l t)
paradoEmCimaTroncoAux (Jogador (x,y)) (Mapa l ((Relva, obstaculos):t)) = False || paradoEmCimaTroncoAux (Jogador (x,y-1)) (Mapa l t)
paradoEmCimaTroncoAux (Jogador (x,y)) (Mapa l ((Rio velocidade, obstaculos):t)) | y == 0 && (obstaculos !! x) == Tronco || paradoEmCimaTroncoAux (Jogador (x,y-1)) (Mapa l t) = True
                                                                                | otherwise = False