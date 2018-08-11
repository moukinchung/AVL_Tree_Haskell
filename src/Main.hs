import System.IO
import Test.QuickCheck

----Código da Arvore AVL----
data TreeAvl a = Vazio | No (TreeAvl a) a (TreeAvl a)
                 deriving (Show, Eq, Ord)

busca :: (Ord a, Eq a) => a -> TreeAvl a -> Bool
busca x Vazio = False
busca x (No l c r) | x == c = True
                   | x <  c = busca x l
                   | x >  c = busca x r

valor :: (Ord a, Eq a) => TreeAvl a -> a --Obtem o valor de filho
valor (No l c r) = c

esquerda :: (Ord a, Eq a) => TreeAvl a-> TreeAvl a--Obtem o nó a esquerda
esquerda Vazio = Vazio
esquerda (No l c r) = l

direita :: (Ord a, Eq a) => TreeAvl a-> TreeAvl a--Obtem o nó a direita
direita Vazio = Vazio
direita (No l c r) = r

rotacaoLL :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a--Rotacao Simples a direita
rotacaoLL Vazio = Vazio
rotacaoLL (No Vazio c Vazio) = (No Vazio c Vazio)
rotacaoLL (No l c r) = No (esquerda l) (valor l) (No (direita l) c r)

rotacaoRR :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a--Rotacao Simples a esquerda
rotacaoRR Vazio = Vazio
rotacaoRR (No Vazio c Vazio) = (No Vazio c Vazio)
rotacaoRR (No l c r) = No (No l c (esquerda r)) (valor r) (direita r)

rotacaoLR :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a--Rotacao Dupla a direita
rotacaoLR Vazio = Vazio
rotacaoLR (No Vazio c Vazio) = (No Vazio c Vazio)
rotacaoLR (No l c r) = No (No (esquerda l) (valor l) (esquerda (direita l)))
                          (valor (direita l))
                          (No (direita (direita l)) c r)

rotacaoRL :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a--Rotacao Dupla a esquerda
rotacaoRL Vazio = Vazio
rotacaoRL (No Vazio c Vazio) = (No Vazio c Vazio)
rotacaoRL (No l c r) = No (No l c (esquerda (esquerda r)))
                          (valor (esquerda r))
                          (No (direita (esquerda r)) (valor r) (direita r))

inserir :: (Ord a, Eq a) => a -> TreeAvl a-> TreeAvl a
inserir x Vazio = No Vazio x Vazio
inserir x (No l c r) | x < c = rebalance(No (inserir x l) c r)
                     | x > c = rebalance(No l c (inserir x r))
                     | otherwise = (No l c r)

altura :: (Ord a, Eq a) => TreeAvl a-> Int
altura Vazio      = 0 -- Se vazio, altura é 0
altura (No l c r) = 1 + (max (altura l) (altura r)) -- Se for um nó, somar um com a maior altura dos filhos

rebalance :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a
rebalance (No l c r) | alturalr == (-2) && altura_r== (-1) = rotacaoRR (No l c r) --Rotacao Simples a esquerda
                     | alturalr == 2    && altura_l== 1    = rotacaoLL (No l c r) --Rotacao Simples a direita
                     | alturalr == (-2) && altura_r== 1    = rotacaoRL (No l c r) --Rotacao esquerda-direita (Dupla a esquerda)---
                     | alturalr == 2    && altura_l== (-1) = rotacaoLR (No l c r) --Rotacao direita-esquerda (Dupla a direita)---
                     | otherwise = (No l c r)
                      where l_l = (esquerda l)
                            l_r = (direita l)
                            r_l = (esquerda r)
                            r_r = (direita r)
                            alturalr = (altura l)-(altura r)
                            altura_r = (altura r_l)-(altura r_r)
                            altura_l = (altura l_l)-(altura l_r)

remover :: (Ord a, Eq a) => a -> TreeAvl a -> TreeAvl a
remover x Vazio              = Vazio
remover x (No Vazio c Vazio) = if x == c then Vazio else (No Vazio c Vazio)

remover x (No l c Vazio)     | x == c = rebalance(l)
                             | x < c = rebalance(No (remover x l) c Vazio)
                             | otherwise = (No l c Vazio)

remover x (No Vazio c r)     | x == c = rebalance(r)
                             | x > c = rebalance(No Vazio c (remover x r))
                             | otherwise = (No Vazio c r)

remover x (No l c r)         | x == c = rebalance(No l c' r')
                             | x < c = rebalance(No (remover x l) c r)
                             | x > c = rebalance(No l c (remover x r))
                             | otherwise = (No l c r)
                               where c' = menorValor r
                                     r' = remover c' r

menorValor :: (Ord a, Eq a) => TreeAvl a -> a
menorValor avl = head(geraLista avl)

geraLista :: (Ord a, Eq a) => TreeAvl a -> [a]
geraLista Vazio = []
geraLista (No l c r) = geraLista l ++ [c] ++ geraLista r

----Testes da Arvore AVL----

geraAVL :: (Ord a, Eq a) => TreeAvl a -> [a] -> TreeAvl a-- Gera uma Arvore AVL a partir de uma lista
geraAVL avl [] = avl
geraAVL avl (x:xs) = inserir x (geraAVL avl xs)


removerAVL :: (Ord a, Eq a) => TreeAvl a -> [a] -> TreeAvl a-- Remove valores de uma AVL a partir de uma lista
removerAVL avl [] = avl
removerAVL avl (x:xs) = remover x (removerAVL avl xs)

----------------------------------
