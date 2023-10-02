{- |
Module      : Tarefa2_2022li1g009
Description : Geração contínua de um mapa
Copyright   : Fábio Magalhães <a104365@alunos.uminho.pt>
              Pedro Rosário <a100536@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.

__Nota:__ @Para a realização desta tarefa, foi necessário utilizar a função @ 'mapaValido' @ da Tarefa 1, que se encontra no módulo 'Tarefa1_2022li1g009'.@

== Bibliotecas utilizadas:
> import System.Random

==== Instalação das bibliotecas:
> $ cabal install --lib random
-}
module Tarefa2_2022li1g009 (
    -- * Funções principais
    -- $principal
    estendeMapa, proximosTerrenosValidos, proximosObstaculosValidos,
    -- * Funções auxiliares
    -- $aux
    velocidadeRandom, chooseRandom, listaObstaculos
    ) where

{- $principal
Conjunto de funções que são executadas para gerar/estender um mapa durante o jogo.
-}

{- $aux
Conjunto de funções auxiliares que realizam tarefas restringem as linhas possiveis para gerar um mapa.
-}

import LI12223

import Tarefa1_2022li1g009 as T

import System.Random -- >>> cabal install --lib random

{- |
A função 'velocidadeRandom' gera um número aleatório entre -1 e 1, que será usado na função 'proximosTerrenosValidos' para gerar um mapa.

=== __Alternativamente a função pode ser definida entre 1 e 3 da seguinte forma:__

@
velocidadeRandom s = (mod (head (randoms (mkStdGen s))) 3) + 1
@

ou entre -2 e 2 da seguinte forma:

@
velocidadeRandom s = (mod (head (randoms (mkStdGen s))) 5) - 2
@

ou:

@
velocidadeRandom s = fst (randomR (-2, 2) (mkStdGen s))
@

== Exemplos de utilização:
>>> velocidadeRandom 1
1

>>> velocidadeRandom 3
-1
-}
velocidadeRandom :: Int -- ^ Seed para gerar um número aleatório
                    -> Int -- ^ Número aleatório entre -1 e 1
velocidadeRandom s | int == 0 = velocidadeRandom (s+1)
                   |otherwise = int
                   where int = fst (randomR ((-1), 1) (mkStdGen s))

{- |
A função 'chooseRandom' escolhe um elemento aleatório de uma lista de acordo com um seed.

=== __A função pode também ser definida da seguinte forma:__

@
chooseRandom s [] = error "Lista vazia"
chooseRandom s xs = xs !! (mod (head (randoms (mkStdGen (s)))) (length xs))
@

== Exemplos de utilização:
>>> chooseRandom 1 [1,2,3]
1

>>> chooseRandom 2 [1,2,3]
3
-}
chooseRandom :: Int -- ^ Seed para gerar um número aleatório
                -> [a] -- ^ Lista de elementos
                -> a -- ^ Elemento aleatório da lista
chooseRandom s [] = error "Lista vazia"
chooseRandom s xs = xs !! (mod (head (randoms (mkStdGen (s+8)))) (length xs)) -- 8 é uma constante qualquer escolhida pois faz a aleatoridade formar um padrão mais divertido para ser jogado

{- |
A função 'estendeMapa' que estende as linhas possiveis à frente do mapa, de forma randomica.

== Exemplos de utilização:
>>> estendeMapa (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum])]) 3
Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]

== Propriedades:
__ Utiliza a função 'chooseRandom' para escolher um elemento aleatório de uma lista de elementos dada pela função 'proximosTerrenosValidos' e em seguida gera os obstáculos possiveis para esse terreno utilizando a função 'listaObstaculos'. __
-}
estendeMapa :: Mapa -- ^ Mapa atual
               -> Int -- ^ Número aleatório que será usado para gerar um mapa
               -> Mapa -- ^ Mapa estendido
estendeMapa m@(Mapa l linhas@((terreno, obstaculos):t)) n = (Mapa l (linhas ++ [proximaLinha]))
    where novoTerreno = chooseRandom (l+n) (proximosTerrenosValidos m)
          proximaLinha = (novoTerreno, listaObstaculos l (novoTerreno, obstaculos) n)

{- |
A função 'listaObstaculos' que gera uma nova lista de obstáculos para uma dada linha com __@l (:: Int)@__ elementos.

== Exemplos de utilização:
>>> listaObstaculos 3 (Relva, [Arvore, Nenhum]) 3
[Arvore,Nenhum,Nenhum]

>>> listaObstaculos 3 (Relva, [Arvore, Nenhum]) 2
[Nenhum,Arvore,Arvore]

== Propriedades:
prop> listaObstaculos 0 (_, []) n = []

__ Utiliza a função 'chooseRandom' para escolher um obstáculo aleatório de uma lista dada pela função 'proximosObstaculosValidos'. __
-}
listaObstaculos :: Int -- ^ Número de elementos da linha pretendida
                   -> (Terreno, [Obstaculo]) -- ^ Terreno e lista de obstáculos da linha anterior
                   -> Int -- ^ Seed para gerar uma lista nova
                   -> [Obstaculo] -- ^ Lista de obstáculos nova
listaObstaculos l linha@(terreno, obstaculos) n | proximosObstaculosValidos l (terreno, (drop l obstaculos)) /= [] = (chooseRandom n (proximosObstaculosValidos l (terreno, (drop l obstaculos)))) : listaObstaculos (l-1) (terreno, (drop l obstaculos)) (n-1*l)
                                                | proximosObstaculosValidos l (terreno, (drop l obstaculos)) /= [] && not (mapaValido (Mapa l [linha])) = (chooseRandom n (proximosObstaculosValidos l (terreno, (drop l obstaculos)))) : take (l-1) (listaObstaculos (l-1) (terreno, (drop l obstaculos)) (n+1*l))
                                                | otherwise = []

{- |
A função 'proximosTerrenosValidos' que determina os próximos terrenos possiveis à frente do mapa.

== Exemplos de utilização:
>>> proximosTerrenosValidos (Mapa 3 [(Relva, [Arvore, Nenhum, Nenhum])])
[Rio 2,Estrada 2,Relva]

== Propriedades:
prop> proximosTerrenosValidos (Mapa l []) = [Rio _, Estrada _, Relva]

__ Utiliza a função 'velocidadeRandom' para gerar um número aleatório entre -3 e 3, que será usado como velocidade do novo terreno do mapa. __
-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l []) = [Rio (velocidadeRandom l), Estrada (velocidadeRandom l), Relva]
proximosTerrenosValidos mapa@(Mapa l ((terreno, obstaculos):t)) = case terreno of
                                                                    Rio v -> if T.mapaContiguoValido mapa && mapaValido mapa then [Rio (velocidadeRandom ((length t)*v*l)), Estrada (velocidadeRandom ((length t)*v*l)), Relva]
                                                                             else [Estrada (velocidadeRandom ((length t)*v*l)), Relva]
                                                                    Estrada v -> if T.mapaContiguoValido mapa && mapaValido mapa then [Estrada (velocidadeRandom ((length t)*v*l)), Rio (velocidadeRandom ((length t)*v*l)), Relva]
                                                                                 else [Rio (velocidadeRandom ((length t)*v*l)), Relva]
                                                                    Relva -> if T.mapaContiguoValido mapa && mapaValido mapa then [Rio (velocidadeRandom ((length t)*l)), Estrada (velocidadeRandom ((length t)*l)), Relva]
                                                                                 else [Estrada (velocidadeRandom ((length t)*l)), Relva]

{- |
A função 'proximosObstaculosValidos' que determina os próximos obstáculos possiveis à frente do mapa.

== Exemplos de utilização:
>>> proximosObstaculosValidos 3 (Relva, [Arvore, Nenhum, Nenhum])
[]

>>> proximosObstaculosValidos 3 (Relva, [Arvore, Nenhum])
[Nenhum, Arvore]

== Propriedades:
prop> proximosObstaculosValidos 0 _ = []
prop> proximosObstaculosValidos l (_, obstaculos) | l == (length obstaculos) = []
-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (Relva, obstaculos) | (length obstaculos) < l = [Nenhum, Arvore] -- proximosObstaculosValidos l (linha ++ chooseRandom [Nenhum, Arvore])
                                                | otherwise = []
proximosObstaculosValidos l (Estrada velocidade, obstaculos) | (length obstaculos) < l = [Nenhum, Carro]
                                                             | otherwise = []
proximosObstaculosValidos l (Rio velocidade, obstaculos) | (length obstaculos) < l = [Nenhum, Tronco]
                                                         | otherwise = []