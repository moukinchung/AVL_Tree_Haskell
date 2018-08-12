Árvore AVL - Implementação em Haskell

1 - Conceitualização

A Árvore Adelson-Velskii e Landis (AVL) é uma árvore de busca binária balanceada que minimiza o número de comparações efetuadas no pior caso para uma busca nessa árvore. Para isso é necessário haver um mecanismo para que seja mantida essa propriedade na árvore a cada inserção ou remoção.
Para essa estrutura de dado, cada elemento da árvore é considerado um nó, e a partir desse nó, haverá dois filhos, que poderão ser um outro nó, ou simplesmente um elemento vazio.
Uma árvore é considerada balanceada se a partir do nó principal, a altura dos seus filhos diferem em módulo de até uma unidade, sendo que essa propriedade também é válida para cada subárvore.

2 - A implementação em Haskell

-O Tipo

Para a implementação na linguagem Haskell, foi criado um novo tipo de dado para representar o nó do elemento com seus dois filhos, além de também poder representar o nó vazio:

data TreeAvl a = Vazio | No (TreeAvl a) a (TreeAvl a)
                            deriving (Show, Eq, Ord)

Como foi definido com o tipo genérico ‘a’, é possível usar essa implementação tanto para tipos numéricos (Int, Integer, Float, Double) ou do tipo caractere (Char, String), assim como do tipo lista ou tupla.

-Função busca

busca :: (Ord a, Eq a) => a -> TreeAvl a -> Bool

A função busca recebe um valor do tipo genérico a e um valor do tipo TreeAvl, e retorna um booleano.
Essa função irá realizar a busca do valor na árvore.
Caso a busca seja feita em uma árvore ou nó vazio, é retornado False
Caso contrário, se o valor a ser procurado é igual ao nó, é retornado True, senão, é realizado a busca recursiva em um de seus filhos, dependendo da comparação da ordem entre os valores.


-Função valor

valor :: (Ord a, Eq a) => TreeAvl a -> a 

A função valor recebe um valor do tipo TreeAvl e retorna um valor do tipo genérico a.
O valor retornado é o do nó recebido.

-Função esquerda

esquerda :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função esquerda recebe um valor do tipo TreeAvl e retorna um valor do tipo TreeAvl.
Caso recebam um Vazio, é retornado um Vazio.
Caso contrário, é retornado o nó à esquerda.

-Função direita

direita :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

As função direita recebe um valor do tipo TreeAvl e retorna um valor do tipo TreeAvl.
Caso recebam um Vazio, é retornado um Vazio.
Caso contrário, é retornado o nó à direita.

-Função rotacaoLL

rotacaoLL :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função rotacaoLL recebe um valor do tipo TreeAvl e retorna um valor do tipo TreeAvl.
Essa função irá executar a rotação LL, ou seja, a rotação simples à direita.
Caso receba um Vazio, é retornado Vazio.
Caso receba um nó com filhos Vazio, é retornado o próprio nó.
Caso contrário, irá retornar o nó rotacionado.

-Função rotacaoRR

rotacaoRR :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função rotacaoRR recebe um valor do tipo TreeAvl e retorna um valor do tipo TreeAvl.
Essa função irá executar a rotação LL, ou seja, a rotação simples à esquerda.
Caso receba um Vazio, é retornado Vazio.
Caso receba um nó com filhos Vazio, é retornado o próprio nó.
Caso contrário, irá retornar o nó rotacionado.

-Função rotacaoLR

rotacaoLR :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função rotacaoLR recebe um valor do tipo TreeAvl e retorna um valor do tipo TreeAvl.
Essa função irá executar a rotação LR, ou seja, a rotação dupla à direita.
Caso receba um Vazio, é retornado Vazio.
Caso receba um nó com filhos Vazio, é retornado o próprio nó.
Caso contrário, irá retornar o nó rotacionado.

-Função rotacaoRL

rotacaoRL :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função rotacaoRL recebe um valor do tipo TreeAvl e retorna um valor do tipo TreeAvl.
Essa função irá executar a rotação RL, ou seja, a rotação dupla à esquerda.
Caso receba um Vazio, é retornado Vazio.
Caso receba um nó com filhos Vazio, é retornado o próprio nó.
Caso contrário, irá retornar o nó rotacionado.

-Função inserir

inserir :: (Ord a, Eq a) => a -> TreeAvl a-> TreeAvl a

A função inserir recebe um valor do tipo genérico a e um valor do tipo TreeAvl e retorna um valor do tipo TreeAvl.
Essa função irá inserir o valor recebido na árvore.
Caso o valor recebido seja inserido em Vazio, será criado um nó com o valor e dois filhos vazios.
Caso contrário, dependendo da ordem entre o valor recebido e do nó, será inserido ou a direita ou à esquerda do nó atual recursivamente, fazendo logo após o rebalanceamento desse nó e então retornado. Se o valor for igual ao nó analisado, é retornado o próprio, não realizando nenhuma inserção.


-Função altura

altura :: (Ord a, Eq a) => TreeAvl a -> Int

A função altura recebe um valor do tipo TreeAvl e retorna um valor do tipo Int.
Essa função irá calcular a altura do nó junto com seus subnós, realizando assim também, o cálculo da altura de uma árvore avl.
Caso se recebe o Vazio é retornado 0.
Caso contrário, é retornado a soma de 1 com o maior valor das alturas dos filhos à esquerda e à esquerda.

-Função rebalance

rebalance :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a


-Função remover

remover :: (Ord a, Eq a) => a -> TreeAvl a -> TreeAvl a

-Função menorValor

menorValor :: (Ord a, Eq a) => TreeAvl a -> a

-Função geraLista

geraLista :: (Ord a, Eq a) => TreeAvl a -> [a]

-Função geraAVL

geraAVL :: (Ord a, Eq a) => TreeAvl a -> [a] -> TreeAvl a-

-Função removerAVL

removerAVL :: (Ord a, Eq a) => TreeAvl a -> [a] -> TreeAvl a

