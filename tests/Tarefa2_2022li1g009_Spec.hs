module Tarefa2_2022li1g009_Spec where

import LI12223
import Tarefa2_2022li1g009
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [
                                                "proximosTerrenosValidos? (2 relvas contiguas)" ~: [Rio (-1), Estrada (-1), Relva] ~=? proximosTerrenosValidos (Mapa 3 [(Relva, [Arvore, Arvore, Nenhum]), (Relva, [Nenhum, Nenhum, Arvore])]),
                                                "proximosTerrenosValidos? (4 rios contiguos)" ~: [Estrada 1,Relva] ~=? proximosTerrenosValidos (Mapa 3 [(Rio 1, [Nenhum, Tronco, Nenhum]), (Rio 1, [Nenhum, Nenhum, Tronco]), (Rio 1, [Tronco, Tronco, Nenhum]), (Rio 1, [Nenhum, Nenhum, Tronco]), (Rio 1, [Nenhum, Nenhum, Tronco])]),
                                                "proximosObstaculosValidos? (Comprimento obstaculos > Comprimento linha)" ~: [] ~=? proximosObstaculosValidos 2 (Estrada 1, [Carro, Nenhum]),
                                                "proximosObstaculosValidos? (Comprimento obstaculos < Comprimento linha)" ~: [Nenhum, Tronco] ~=? proximosObstaculosValidos 3 (Rio 1, []),
                                                "estendeMapa?" ~: Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Estrada (-1),[Nenhum,Carro,Carro])] ~=? estendeMapa (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum])]) 2
                                             ]
