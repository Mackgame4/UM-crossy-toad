module Tarefa5_2022li1g009_Spec where

import LI12223
import Tarefa5_2022li1g009
import Test.HUnit

testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test [
                                                "deslizaJogo (n=1)?" ~: Jogo (Jogador (1,-1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro])]) ~=? deslizaJogo 1 (Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])),
                                                "deslizaJogo (n=2)?" ~: Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro]),(Rio (-1),[Nenhum,Tronco,Tronco])]) ~=? deslizaJogo 2 (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro])])),
                                                "animaDeslizaJogo 1 vez?" ~: Jogo (Jogador (1,0)) (Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum])])  ~=? animaDeslizaJogo 1 (Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) 1,
                                                "animaDeslizaJogo 2 vezes?" ~: Jogo (Jogador (1,0)) (Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum])]) ~=? animaDeslizaJogo 1 (Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) 2
                                             ]
