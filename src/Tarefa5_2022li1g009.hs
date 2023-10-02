{- |
Module      : Tarefa5_2022li1g009
Description : Deslize do mapa do jogo para a frente
Copyright   : Fábio Magalhães <a104365@alunos.uminho.pt>
              Pedro Rosário <a100536@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g009 where

import LI12223
import Tarefa2_2022li1g009 as T

{- |
Função 'deslizaJogo' que faz com que a última linha do mapa desapareça, ao mesmo tempo que uma nova linha no topo do mapa seja criada (utilizando a função "estendeMapa" da Tarefa 2).

== Exemplos de utilização:
>>> deslizaJogo 1 (Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]))
Jogo (Jogador (1,-1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro])])

>>> deslizaJogo 2 (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro])]))
Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro]),(Rio (-1),[Nenhum,Tronco,Tronco])])
-}
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo n (Jogo jogador@(Jogador (x,y)) mapa@(Mapa l linhas)) = Jogo (Jogador (x,y-1)) (T.estendeMapa (Mapa l (tail linhas)) n)

{- |
Função 'animaDeslizaJogo' que desliza o jogo se o jogador estiver à frente de metade do mapa linha do mapa _@n@_ vezes.

== Exemplos de utilização:
>>> animaDeslizaJogo 1 (Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) 1
Jogo (Jogador (1,0)) (Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum])])

>>> animaDeslizaJogo 1 (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro])])) 3
Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro])])
-}
animaDeslizaJogo :: Int -> Jogo -> Int -> Jogo
animaDeslizaJogo n jogo@(Jogo jogador@(Jogador (x,y)) mapa@(Mapa l linhas)) t | y > (div l 2) = animaDeslizaJogo n (deslizaJogo n jogo) (t+1)
                                                                              | otherwise = jogo