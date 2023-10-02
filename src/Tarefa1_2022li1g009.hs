{- |
Module      : Tarefa1_2022li1g009
Description : Validação de um mapa
Copyright   : Fábio Magalhães <a104365@alunos.uminho.pt>
              Pedro Rosário <a100536@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g009 (
    -- * Tipos
    Linha,
    -- * Funções principais
    -- $principal
    mapaValido, lengthMapa, obstaculosValidos, linhaValida, doisRios, troncosValidos, carrosValidos, mapaContiguoValido,
    -- * Funções auxiliares
    -- $aux
    obstaculosValidosAux, troncosValidosAux, carrosValidosAux
    ) where

{- $principal
Conjunto de funções que são executadas para validar um mapa durante o jogo.
-}

{- $aux
Conjunto de funções auxiliares que realizam tarefas específicas para a validação de um mapa.
-}

import LI12223

-- 2.1
{- |
A função 'mapaValido' que verifica se um mapa é válido.

==== __Alternativamente, a função poderia ser definida da seguinte forma:__

@
mapaValido mapa@(Mapa l m@((terreno, obstaculos):t)) | l > 0 && m /= [] && (lenghtMapa l m) && (obstaculosValidos m) && (linhaValida obstaculos) && (doisRios mapa) && (troncosValidos mapa) && (carrosValidos mapa) && (mapaContiguoValido mapa) = True
                                                     | otherwise = False
@

== Exemplos de utilização:
>>> mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])
True

== Propriedades:
prop> mapaValido (Mapa _ []) = False
prop> mapaValido (Mapa 0 _) = False
-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = False
mapaValido (Mapa 0 _) = False
mapaValido mapa@(Mapa l m@((terreno, obstaculos):t)) = (lengthMapa l m) && (obstaculosValidos m) && (linhaValida m) && (doisRios mapa) && (troncosValidos mapa) && (carrosValidos mapa) && (mapaContiguoValido mapa)

-- 2.1.6
{- |
A função 'lengthMapa' que verifica se o comprimento de um mapa é igualizado pelo número de obstaculos de cada linha.

== Exemplos de utilização:
>>> lengthMapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]
True

>>> lengthMapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore])]
False
-}
lengthMapa :: Int -> [Linha] -> Bool
lengthMapa l [] = True
lengthMapa l ((terreno, obstaculos):t) | l == length obstaculos = lengthMapa l t
                                       | otherwise = False

-- 2.1.1
type Linha = (Terreno, [Obstaculo]) -- ^ Tipo que representa uma linha de um mapa.

{- |
A função 'obstaculosValidos' que verifica se os obstáculos de uma linha são válidos.

== Exemplos de utilização:
>>> obstaculosValidos [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]
True

>>> obstaculosValidos [(Relva, [Carro, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]
False

== Propriedades:
prop> obstaculosValidos [] = True

__Utiliza a função 'obstaculosValidosAux' para verificar se os obstáculos de uma linha são válidos.__
-}
obstaculosValidos :: [Linha] -> Bool
obstaculosValidos [] = True
obstaculosValidos ((terreno, obstaculos):t) = (obstaculosValidosAux terreno obstaculos) && (obstaculosValidos t)

{- |
Função 'obstaculosValidosAux' auxiliar que verifica se um obstáculo devia estar colocado num determinado terreno.

== Exemplos de utilização:
>>> obstaculosValidosAux Relva [Arvore, Nenhum, Arvore, Nenhum, Arvore]
True

>>> obstaculosValidosAux Relva [Carro, Nenhum, Arvore, Nenhum, Arvore]
False

== Propriedades:
prop> obstaculosValidosAux _ [] = True
prop> obstaculosValidosAux Relva [Arvore, Nenhum] = True
prop> obstaculosValidosAux (Estrada _) [Carro, Nenhum] = True
prop> obstaculosValidosAux (Rio _) [Tronco, Nenhum] = True
-}
obstaculosValidosAux :: Terreno -> [Obstaculo] -> Bool
obstaculosValidosAux _ [] = True
obstaculosValidosAux terreno@Relva (o:t) | o == Arvore || o == Nenhum = obstaculosValidosAux terreno t
                                         | otherwise = False
obstaculosValidosAux terreno@(Estrada velocidade) (o:t) | o == Carro || o == Nenhum = obstaculosValidosAux terreno t
                                                        | otherwise = False
obstaculosValidosAux terreno@(Rio velocidade) (o:t) | o == Tronco || o == Nenhum = obstaculosValidosAux terreno t
                                                    | otherwise = False

-- 2.1.5
{- |
A função 'linhaValida' que verifica se todas as linhas de um mapa, relva e estradas tem um espaço livre. E se rios tem pelo menos um tronco pelo qual o jogador pode passar.

== Exemplos de utilização:
>>> linhaValida [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]
True

>>> linhaValida [(Relva, [Arvore, Arvore, Arvore, Arvore, Arvore]) ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]) ,(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]
False

== Propriedades:
prop> linhaValida [] = True
-}
linhaValida :: [Linha] -> Bool
linhaValida [] = True
linhaValida ((Relva, obstaculos):t) = (elem Nenhum obstaculos) && (linhaValida t)
linhaValida ((Estrada v, obstaculos):t) = (elem Nenhum obstaculos) && (linhaValida t)
linhaValida ((Rio v, obstaculos):t) = (elem Nenhum obstaculos) && (elem Tronco obstaculos) && (linhaValida t)

-- 2.1.2
{- |
A função 'doisRios' que verifica se um mapa tem dois rios consecutivos não tem a mesma velocidade (direção).

== Exemplos de utilização:
>>> doisRios (Mapa 5 [(Rio 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])])
False

>>> doisRios (Mapa 5 [(Rio 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum]) ,(Rio (-1), [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])])
True

== Propriedades:
prop> doisRios (Mapa _ []) = True
prop> doisRios (Mapa _ (x:[])) = True
-}
doisRios :: Mapa -> Bool
doisRios (Mapa _ []) = True
doisRios (Mapa _ (x:[])) = True
doisRios (Mapa _ ((Rio velocidade1, obstaculos1):(Rio velocidade2, obstaculos2):t)) | velocidade1 > 0 && velocidade2 < 0 || velocidade1 < 0 && velocidade2 > 0 = True -- ou: velocidade1*velocidade2 > 0
                                                                                    | otherwise = False
doisRios (Mapa _ (x:y:t)) = doisRios (Mapa 0 (y:t))

-- 2.1.3
{- |
A função 'troncosValidos' que verifica se troncos tem, no maximo, 5 unidades de comprimento.

== Exemplos de utilização:
>>> troncosValidos (Mapa 5 [(Rio 1, [Tronco, Tronco, Nenhum, Tronco, Nenhum]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])])
True

>>> troncosValidos (Mapa 5 [(Rio 1, [Tronco, Tronco, Tronco, Tronco, Tronco]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])])
False

== Propriedades:
prop> troncosValidos (Mapa _ []) = True

__Utiliza a função 'troncosValidosAux' para verificar se os troncos de uma linha são válidos.__
-}
troncosValidos :: Mapa -> Bool
troncosValidos (Mapa _ []) = True
troncosValidos (Mapa _ ((Rio velocidade, obstaculos):t)) = troncosValidos (Mapa 0 t) && troncosValidosAux obstaculos
troncosValidos (Mapa _ (x:t)) = troncosValidos (Mapa 0 t)

{- |
Função 'troncosValidosAux' auxiliar que verifica na linha não existem 5 troncos consecutivos.

== Exemplos de utilização:
>>> troncosValidosAux [Tronco, Tronco, Nenhum, Tronco, Nenhum]
True

>>> troncosValidosAux [Tronco, Tronco, Tronco, Tronco, Tronco]
False

== Propriedades:
prop> troncosValidosAux [] = True
prop> troncosValidosAux [Tronco, Tronco, Nenhum, Tronco, Nenhum] = True
prop> troncosValidosAux [Tronco, Tronco, Tronco, Tronco, Tronco] = False
-}
troncosValidosAux :: [Obstaculo] -> Bool
troncosValidosAux [] = True
troncosValidosAux (o:t) = if filter (== Tronco) (take 6 (o:t)) == [Tronco, Tronco, Tronco, Tronco, Tronco] then False else troncosValidosAux t

-- 2.1.4
{- |
A função 'carrosValidos' que verifica se carros tem, no maximo, 3 unidades de comprimento.

== Exemplos de utilização:
>>> carrosValidos (Mapa 5 [(Estrada 1, [Carro, Carro, Nenhum, Carro, Nenhum]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])])
True

>>> carrosValidos (Mapa 5 [(Estrada 1, [Carro, Carro, Carro, Carro, Carro]) ,(Rio 1, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])])
False

== Propriedades:
prop> carrosValidos (Mapa _ []) = True

__Utiliza a função 'carrosValidosAux' para verificar se os carros de uma linha são válidos.__
-}
carrosValidos :: Mapa -> Bool
carrosValidos (Mapa _ []) = True
carrosValidos (Mapa _ ((Estrada velocidade, obstaculos):t)) = carrosValidos (Mapa 0 t) && carrosValidosAux obstaculos
carrosValidos (Mapa _ (x:t)) = carrosValidos (Mapa 0 t)

{- |
Função 'carrosValidosAux' auxiliar que verifica se na linha não existem 3 carros consecutivos.

== Exemplos de utilização:
>>> carrosValidosAux [Carro, Carro, Nenhum, Carro, Nenhum]
True

>>> carrosValidosAux [Carro, Carro, Carro, Carro, Carro]
False

== Propriedades:
prop> carrosValidosAux [] = True
prop> carrosValidosAux [Carro, Carro, Nenhum] = True
prop> carrosValidosAux [Carro, Carro, Carro] = False
-}
carrosValidosAux :: [Obstaculo] -> Bool
carrosValidosAux [] = True
carrosValidosAux (o:t) = if filter (== Carro) (take 4 (o:t)) == [Carro, Carro, Carro] then False else carrosValidosAux t

-- 2.1.7
{- |
A função 'mapaContiguoValido' que verifica que não devem existir mais do que 4 rios continuos, nem 5 estradas ou relvas.

== Exemplos de utilização:
>>> mapaContiguoValido (Mapa 3 [(Rio 1, [Nenhum, Nenhum, Nenhum]), (Rio 1, [Nenhum, Nenhum, Nenhum]), (Rio 1, [Nenhum, Nenhum, Nenhum]), (Rio 1, [Nenhum, Nenhum, Nenhum]), (Rio 1, [Nenhum, Nenhum, Nenhum])])
False

>>> mapaContiguoValido (Mapa 3 [(Rio 1, [Nenhum, Nenhum, Nenhum]), (Rio 1, [Nenhum, Nenhum, Nenhum]), (Rio 1, [Nenhum, Nenhum, Nenhum]), (Rio 1, [Nenhum, Nenhum, Nenhum])])
True

== Propriedades:
prop> mapaContiguoValido (Mapa _ []) = True
prop> mapaContiguoValido (Mapa _ (x:[])) = True
-}
mapaContiguoValido :: Mapa -> Bool
mapaContiguoValido (Mapa _ []) = True
mapaContiguoValido (Mapa _ (x:[])) = True
mapaContiguoValido (Mapa _ ((Rio velocidade1, obstaculos1):(Rio velocidade2, obstaculos2):(Rio velocidade3, obstaculos3):(Rio velocidade4, obstaculos4):(Rio velocidade5, obstaculos5):t)) = False
mapaContiguoValido (Mapa _ ((Estrada velocidade1, obstaculos1):(Estrada velocidade2, obstaculos2):(Estrada velocidade3, obstaculos3):(Estrada velocidade4, obstaculos4):(Estrada velocidade5, obstaculos5):(Estrada velocidade6, obstaculos6):t)) = False
mapaContiguoValido (Mapa _ ((Relva, obstaculos1):(Relva, obstaculos2):(Relva, obstaculos3):(Relva, obstaculos4):(Relva, obstaculos5):(Relva, obstaculos6):t)) = False
mapaContiguoValido (Mapa _ (x:y:t)) = mapaContiguoValido (Mapa 0 (y:t))