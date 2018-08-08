import System.IO
import Test.QuickCheck

----Código da Arvore AVL----
data TreeAvl = Vazio | No (TreeAvl) Int (TreeAvl)
                 deriving (Show, Eq)

busca :: Int -> TreeAvl -> Bool
busca x Vazio = False
busca x (No l c r) | x == c = True -- Se o elemento for igual ao nó, sucesso na busca
                   | x <  c = busca x l -- Se elemento for menor ao nó, busca-se no filho a esquerda
                   | x >  c = busca x r -- Se elemento for maior ao nó, busca-se no filho a direita

altura :: TreeAvl -> Int
altura Vazio      = 0 -- Se vazio, altura é 0
altura (No l c r) = 1 + (max (altura l) (altura r)) -- Se for um nó, somar um com a maior altura dos filhos

balanceado :: TreeAvl -> Bool
balanceado Vazio = True
balanceado (No l c r) | abs((altura l) - (altura r)) < 2 = True
                      | otherwise = False

valor :: TreeAvl -> Int --Obtem o valor de filho
valor Vazio = 0
valor (No l c r) = c

esquerda :: TreeAvl -> TreeAvl --Obtem o nó a esquerda
esquerda Vazio = Vazio
esquerda (No l c r) = l

direita :: TreeAvl -> TreeAvl --Obtem o nó a direita
direita Vazio = Vazio
direita (No l c r) = r

rotacaoLL :: TreeAvl -> TreeAvl --Rotacao Simples a direita
rotacaoLL Vazio = Vazio
rotacaoLL (No l c r) = No (esquerda l) (valor l) (No (direita l) c r)

rotacaoRR :: TreeAvl -> TreeAvl --Rotacao Simples a esquerda
rotacaoRR Vazio = Vazio
rotacaoRR (No l c r) = No (No l c (esquerda r)) (valor r) (direita r)

rotacaoLR :: TreeAvl -> TreeAvl --Rotacao esquerda-direita (Dupla a esquerda)
rotacaoLR Vazio = Vazio
rotacaoLR (No l c r) = No (No (esquerda l) (valor l) (esquerda (direita l)))
                          (valor (direita l))
                          (No (direita (direita l)) c r)

rotacaoRL :: TreeAvl -> TreeAvl --Rotacao direita-esquerda (Dupla a direita)
rotacaoRL Vazio = Vazio
rotacaoRL (No l c r) = No (No l c (esquerda (esquerda r)))
                          (valor (esquerda r))
                          (No (direita (esquerda r)) (valor r) (direita r))

inserir :: Int -> TreeAvl -> TreeAvl
inserir x Vazio = No Vazio x Vazio
inserir x (No l c r) | x < c = rebalance(No (inserir x l) c r)
                     | x > c = rebalance(No l c (inserir x r))
                     | otherwise = (No l c r)

rebalance :: TreeAvl -> TreeAvl
rebalance (No l c r) | balanceado (No l c r) == True = (No l c r)
                     | alturalr == (-2) && altura_r==(-1) = rotacaoRR (No l c r) --Rotacao Simples a esquerda
                     | alturalr == 2    && altura_l==1    = rotacaoLL (No l c r) --Rotacao Simples a direita
                     | alturalr == (-2) && altura_r==1    = rotacaoLR (No l c r) --Rotacao esquerda-direita (Dupla a esquerda)
                     | alturalr == 2    && altura_l==(-1) = rotacaoRL (No l c r) --Rotacao direita-esquerda (Dupla a direita)
                     | otherwise = (No l c r)
                      where l_l = (esquerda l)
                            l_r = (direita l)
                            r_l = (esquerda r)
                            r_r = (direita r)
                            alturalr = (altura l)-(altura r)
                            altura_r = (altura r_l)-(altura r_r)
                            altura_l = (altura l_l)-(altura l_r)

remover :: Int -> TreeAvl -> TreeAvl
remover x Vazio              = Vazio
remover x (No Vazio c Vazio) = if x == c then Vazio else (No Vazio c Vazio)
remover x (No l c Vazio)     = if x == c then l else (No l c Vazio)
remover x (No Vazio c r)     = if x == c then r else (No Vazio c r)
remover x (No l c r)         | x == c = rebalance(No l c' r')
                             | x < c = rebalance(No (remover x l) c r)
                             | x > c = rebalance(No l c (remover x r))
                             | otherwise = (No l c r)
                               where c' = menorValor r
                                     r' = remover c' r

menorValor :: TreeAvl -> Int
menorValor avl = head(geraLista avl)

geraLista :: TreeAvl -> [Int]
geraLista Vazio = []
geraLista (No l c r) = geraLista l ++ [c] ++ geraLista r

----Testes da Arvore AVL----

geraAVL :: TreeAvl -> [Int] -> TreeAvl -- Gera uma Arvore AVL a partir de uma lista
geraAVL t [] = t
geraAVL t (x:xs) = inserir x (geraAVL t xs)

----------------------------------
