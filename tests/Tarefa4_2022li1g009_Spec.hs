module Tarefa4_2022li1g009_Spec where

import LI12223
import Tarefa4_2022li1g009
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [
                                                "emAgua?" ~: True ~=? emAgua (Jogador (2,1)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]),
                                                "jogadorAtropelado?" ~: True ~=? jogadorAtropelado (Jogador (0,2)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]),
                                                "jogoTerminou? (Jogador Atropelado)" ~: True ~=? jogoTerminou (Jogo (Jogador (1,2)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Nenhum, Carro, Nenhum])])),
                                                "jogoTerminou?" ~: False ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Rio 1, [Nenhum, Tronco, Nenhum]), (Estrada 1, [Carro, Nenhum, Nenhum])]))
                                             ]
