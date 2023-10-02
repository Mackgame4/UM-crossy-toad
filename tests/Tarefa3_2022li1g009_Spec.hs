module Tarefa3_2022li1g009_Spec where

import LI12223
import Tarefa3_2022li1g009
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [
                                                "animaEstradaRio?" ~: [(Rio 1,[Nenhum,Tronco,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Carro,Nenhum,Carro])] ~=? animaEstradaRio [(Rio 1, [Tronco, Nenhum, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Carro, Nenhum])],
                                                "paradoEmCimaTronco? (Parado fora de um tronco)" ~: (Jogador (1,1)) ~=? paradoEmCimaTronco (Jogador (1,1)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]),
                                                "paradoEmCimaTronco? (Parado num tronco)" ~: (Jogador (2,0)) ~=? paradoEmCimaTronco (Jogador (1,0)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]),
                                                "paradoEmCimaTronco? (Parado num tronco, velocidade >0)" ~: (Jogador (2,1)) ~=? paradoEmCimaTronco (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]), (Rio 1, [Nenhum, Tronco, Nenhum])]),
                                                "paradoEmCimaTronco? (Parado num tronco, velocidade <0)" ~: (Jogador (1,1)) ~=? paradoEmCimaTronco (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]), (Rio (-3), [Nenhum, Tronco, Nenhum])]),
                                                "naoMoveCimaArvore? (Em cima de uma estrada)" ~: True ~=? naoMoveCimaArvore (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]),
                                                "naoMoveCimaArvore? (Em cima de uma árvore)" ~: False ~=? naoMoveCimaArvore (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]), (Relva, [Nenhum, Arvore, Nenhum])]),
                                                "animaJogador? (Parado num tronco)" ~: (Jogador (1,0)) ~=? animaJogador (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) Parado,
                                                "animaJogador? (Move Cima)" ~: (Jogador (1,1)) ~=? animaJogador (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) (Move Cima),
                                                "animaJogador? (Move Baixo) para fora do mapa" ~: (Jogador (1,0)) ~=? animaJogador (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) (Move Baixo),
                                                "animaJogador? (Move Esquerda) para fora do mapa" ~: (Jogador (0,1)) ~=? animaJogador (Jogo (Jogador (0,1)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) (Move Esquerda),
                                                "animaJogo? (Move Cima) e anima rio e estrada" ~: (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Nenhum, Carro, Nenhum])])) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) (Move Cima),
                                                "animaJogo? (Parado) e anima rio e estrada" ~: (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Nenhum, Carro, Nenhum])])) ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])])) (Parado),
                                                "animaJogo? (Parado num tronco) e anima rio e estrada" ~: (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 2, [Nenhum, Nenhum, Carro])])) ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 1, [Tronco, Nenhum, Nenhum]), (Estrada 2, [Carro, Nenhum, Nenhum])])) (Parado)
                                             ]
