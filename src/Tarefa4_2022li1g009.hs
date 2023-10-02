{- |
Module      : Tarefa4_2022li1g009
Description : Determinar se o jogo terminou
Copyright   : Fábio Magalhães <a104365@alunos.uminho.pt>
              Pedro Rosário <a100536@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g009 (
    -- * Funções principais
    -- $principal
    jogoTerminou,
    -- * Funções auxiliares
    -- $aux
    foraMapa, emAgua, jogadorAtropelado
    ) where

{- $principal
Conjunto de funções que são executadas para validar que um jogo terminou.
-}

{- $aux
Conjunto de funções auxiliares que realizam tarefas específicas para a validação de que um jogo terminou.
-}

import LI12223

{- |
A função 'jogoTerminou' recebe o estado do jogo (posição do 'Jogador' e obstáculos do 'Mapa') e determina se o jogo terminou.

== Exemplos de utilização:
>>> jogoTerminou (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Estrada 1, [Nenhum, Carro, Nenhum])]))
True

>>> jogoTerminou (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Estrada 1, [Nenhum, Nenhum, Nenhum])]))
False
-}
jogoTerminou :: Jogo -> Bool
jogoTerminou jogo@(Jogo jogador@(Jogador (x,y)) mapa@(Mapa l linhas)) | foraMapa jogador mapa || emAgua jogador mapa || jogadorAtropelado jogador mapa = True
                                                                      | otherwise = False

{- |
A função 'foraMapa' que verifica se o jogador está fora do mapa.

== Exemplos de utilização:
>>> foraMapa (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Estrada 1, [Nenhum, Carro, Nenhum])])
False

>>> foraMapa (Jogador (1,2)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Estrada 1, [Nenhum, Nenhum, Nenhum])])
True
-}
foraMapa :: Jogador -> Mapa -> Bool
foraMapa (Jogador (x,y)) (Mapa l linhas) | x < 0 || x >= l || y < 0 || y >= (length linhas) = True
                                         | otherwise = False

-- | Função que verifica se o jogador está na água (Nenhum obstaculo na posição do jogador num rio).
{- |
A função 'emAgua' que verifica se o jogador está na água ('Nenhum' obstaculo na posição do jogador num rio).

== Exemplos de utilização:
>>> emAgua (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Rio 1, [Nenhum, Nenhum, Nenhum])])
True

>>> emAgua (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Nenhum])])
False

== Propriedades:
prop> emAgua (Jogador (x,y)) (Mapa l []) = False
-}
emAgua :: Jogador -> Mapa -> Bool
emAgua (Jogador (x,y)) (Mapa l []) = False
emAgua (Jogador (x,y)) (Mapa l ((Estrada velocidade, obstaculos):t)) = False || emAgua (Jogador (x,y-1)) (Mapa l t)
emAgua (Jogador (x,y)) (Mapa l ((Relva, obstaculos):t)) = False || emAgua (Jogador (x,y-1)) (Mapa l t)
emAgua (Jogador (x,y)) (Mapa l ((Rio velocidade, obstaculos):t)) | y == 0 && (obstaculos !! x) == Nenhum || emAgua (Jogador (x,y-1)) (Mapa l t) = True
                                                                 | otherwise = False

{- |
A função 'jogadorAtropelado' que verifica se o jogador está "debaixo" de um carro (i.e. na mesma posição de um carro).

== Exemplos de utilização:
>>> jogadorAtropelado (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Estrada 1, [Nenhum, Carro, Nenhum])])
True

>>> jogadorAtropelado (Jogador (1,1)) (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum]),(Estrada 1, [Nenhum, Nenhum, Nenhum])])
False

== Propriedades:
prop> jogadorAtropelado (Jogador (x,y)) (Mapa l []) = False
-}
jogadorAtropelado :: Jogador -> Mapa -> Bool
jogadorAtropelado (Jogador (x,y)) (Mapa l []) = False
jogadorAtropelado (Jogador (x,y)) (Mapa l ((Rio velocidade, obstaculos):t)) = False || jogadorAtropelado (Jogador (x,y-1)) (Mapa l t)
jogadorAtropelado (Jogador (x,y)) (Mapa l ((Relva, obstaculos):t)) = False || jogadorAtropelado (Jogador (x,y-1)) (Mapa l t)
jogadorAtropelado (Jogador (x,y)) (Mapa l ((Estrada velocidade, obstaculos):t)) | y == 0 && (obstaculos !! x) == Carro || jogadorAtropelado (Jogador (x,y-1)) (Mapa l t) = True
                                                                                | otherwise = False