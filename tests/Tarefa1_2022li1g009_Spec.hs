module Tarefa1_2022li1g009_Spec where

import LI12223
import Tarefa1_2022li1g009
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test [
                                                "Mapa válido? (Velocidade -1)" ~: True ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]),
                                                "Mapa válido? (Velocidade 3)" ~: True ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada 3, [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]),
                                                "Mapa válido? (Comprimento do mapa inferior aos obstaculos)" ~: False ~=? mapaValido (Mapa 4 [(Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore])]),
                                                "Mapa válido? (Obstaculos inválidos)" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Carro, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]),
                                                "Mapa válido? (Linha sem espaços de movimentação)" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Arvore, Arvore, Arvore, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]),
                                                "Mapa válido? (Dois rios contiguos em direções opostas)" ~: True ~=? mapaValido (Mapa 5 [(Rio 1, [Tronco, Nenhum, Nenhum, Nenhum, Nenhum]) ,(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]),
                                                "Mapa válido? (Dois rios contiguos em direções iguais)" ~: False ~=? mapaValido (Mapa 5 [(Rio 1, [Tronco, Nenhum, Nenhum, Nenhum, Nenhum]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Tronco, Tronco]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]),
                                                "Mapa válido? (Troncos com mais de 5 de comprimento)" ~: False ~=? mapaValido (Mapa 6 [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Tronco, Tronco, Tronco]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore])]),
                                                "Mapa válido? (Carros com mais de 3 de comprimento)" ~: False ~=? mapaValido (Mapa 6 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore]) ,(Estrada 1, [Carro, Carro, Carro, Carro, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore, Arvore])]),
                                                "Mapa válido? (Mais de 4 rios contiguos)" ~: False ~=? mapaValido (Mapa 5 [(Rio 1, [Tronco, Nenhum, Nenhum, Nenhum, Nenhum]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Tronco, Tronco]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Tronco, Tronco]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Tronco, Tronco]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Tronco, Tronco])])
                                             ]
