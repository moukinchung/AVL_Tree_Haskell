module Main where

import System.IO

data TreeAvl = Vazio | No (TreeAvl) Int (TreeAvl)
                 deriving (Show, Eq)

busca :: Int -> TreeAvl -> Bool
busca x Vazio = False
busca x (No l c r) | x == c = True -- Se o elemento for igual ao nó, sucesso na busca
                   | x <  c = busca x l -- Se elemento for menor ao nó, busca-se no filho a esquerda
                   | x >  c = busca x r -- Se elemento for maior ao nó, busca-se no filho a direita

altura :: TreeAvl -> Int
altura Vazio      = -1 -- Se vazio, altura é -1
altura (No l c r) = 1 + (max (altura l) (altura r)) -- Se for um nó, somar um com a maior altura dos filhos

balanceado :: TreeAvl -> Bool
balanceado Vazio = True
balanceado (No l c r) | abs((altura l) - (altura r)) < 2 = True
                      | otherwise = False

valor :: TreeAvl -> Int
valor Vazio = 0
valor (No l c r) = c

esquerda :: TreeAvl -> TreeAvl
esquerda Vazio = Vazio
esquerda (No l c r) = l

direita :: TreeAvl -> TreeAvl
direita Vazio = Vazio
direita (No l c r) = r

rotacaoLL :: TreeAvl -> TreeAvl
rotacaoLL Vazio = Vazio
rotacaoLL (No l c r) = No (esquerda l) (valor l) (No (direita l) c r)

rotacaoRR :: TreeAvl -> TreeAvl
rotacaoRR Vazio = Vazio
rotacaoRR (No l c r) = No (No l c (esquerda r)) (valor r) (direita r)

rotacaoLR :: TreeAvl -> TreeAvl
rotacaoLR Vazio = Vazio
rotacaoLR (No l c r) = No (No (esquerda l) (valor l) (esquerda (direita l)))
                          (valor (direita l))
                          (No (direita (direita l)) c r)

rotacaoRL :: TreeAvl -> TreeAvl
rotacaoRL Vazio = Vazio
rotacaoRL (No l c r) = No (No l c (esquerda (esquerda r)))
                          (valor (esquerda r))
                          (No (direita (esquerda r)) (valor r) (direita r))

inserir :: Int -> TreeAvl -> TreeAvl
inserir x Vazio = No Vazio x Vazio


remover :: Int -> TreeAvl -> TreeAvl
remover x Vazio              = Vazio
remover x (No Vazio c Vazio) = if x == c then Vazio else (No Vazio c Vazio)
remover x (No l c Vazio)     = if x == c then l else (No l c Vazio)
remover x (No Vazio c r)     = if x == c then r else (No Vazio c r)
