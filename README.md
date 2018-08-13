# **Árvore AVL - Implementação em Haskell**

## 1 - Conceitualização

A Árvore Adelson-Velskii e Landis (AVL) é uma árvore de busca binária balanceada que minimiza o número de comparações efetuadas no pior caso para uma busca nessa árvore.

Para isso é necessário haver um mecanismo para que seja mantida essa propriedade na árvore a cada inserção ou remoção.

Para essa estrutura de dado, cada elemento da árvore é considerado um nó, e a partir desse nó, haverá dois filhos, que poderão ser um outro nó, ou simplesmente um elemento vazio.

Uma árvore é considerada balanceada se a partir do nó principal, a altura dos seus filhos diferem em módulo de até uma unidade, sendo que essa propriedade também é válida para cada subárvore.

## 2 - A implementação em Haskell

### O tipo

Para a implementação na linguagem Haskell, foi criado um novo tipo de dado para representar o nó do elemento com seus dois filhos, além de também poder representar o nó vazio:

    data TreeAvl a = Vazio | No (TreeAvl a) a (TreeAvl a)
                     deriving (Show, Eq, Ord)

### Função busca

    busca :: (Ord a, Eq a) => a -> TreeAvl a -> Bool

A função *busca* recebe um valor do tipo genérico *a* e um valor do tipo **TreeAvl**, e retorna um **bool**.

Essa função irá realizar a busca do valor na árvore.

Caso a busca seja feita em uma árvore ou nó vazio, é retornado **False**.

Caso contrário, se o valor a ser procurado é igual ao nó, é retornado **True**, senão, é realizado a busca recursiva em um de seus filhos, dependendo da comparação da ordem entre os valores.

### Função valor

    valor :: (Ord a, Eq a) => TreeAvl a -> a

A função *valor* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo genérico **a**.

O valor retornado é o do nó recebido.

### Função esquerda

    esquerda :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a
A função *esquerda* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Caso recebam um **Vazio**, é retornado um **Vazio**.

Caso contrário, é retornado o nó à esquerda.

### Função direita

    direita :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função *direita* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Caso recebam um **Vazio**, é retornado um **Vazio**.

Caso contrário, é retornado o nó à direita.

### Função rotacaoLL

    rotacaoLL :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função *rotacaoLL* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Essa função irá executar a rotação LL, ou seja, a rotação simples à direita.

Caso receba um **Vazio**, é retornado **Vazio**.

Caso receba um nó com filhos **Vazio**, é retornado o próprio nó.

Caso contrário, irá retornar o nó rotacionado.

### Função rotacaoRR

    rotacaoRR :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função *rotacaoRR* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Essa função irá executar a rotação RR, ou seja, a rotação simples à esquerda.

Caso receba um **Vazio**, é retornado **Vazio**.

Caso receba um nó com filhos **Vazio**, é retornado o próprio nó.

Caso contrário, irá retornar o nó rotacionado.

### Função rotacaoLR

    rotacaoLR :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função *rotacaoLR* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Essa função irá executar a rotação LR, ou seja, a rotação dupla à direita.

Caso receba um **Vazio**, é retornado **Vazio**.

Caso receba um nó com filhos **Vazio**, é retornado o próprio nó.

Caso contrário, irá retornar o nó rotacionado.

### Função rotacaoRL

    rotacaoRL :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função *rotacaoRL* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Essa função irá executar a rotação RL, ou seja, a rotação dupla à esquerda.

Caso receba um **Vazio**, é retornado **Vazio**.

Caso receba um nó com filhos **Vazio**, é retornado o próprio nó.

Caso contrário, irá retornar o nó rotacionado.

### Função inserir

    inserir :: (Ord a, Eq a) => a -> TreeAvl a -> TreeAvl a

A função *inserir* recebe um valor do tipo genérico **a** e um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Essa função irá inserir o valor recebido na árvore.

Caso o valor recebido seja inserido em **Vazio**, será criado um nó com o valor e dois filhos vazios.

Caso contrário, dependendo da ordem entre o valor recebido e do nó, será inserido ou a direita ou à esquerda do nó atual recursivamente, fazendo logo após o rebalanceamento desse nó e então retornado. 

Se o valor for igual ao nó analisado, é retornado o próprio, não realizando nenhuma inserção.

### Função altura

    altura :: (Ord a, Eq a) => TreeAvl a -> Int**==

A função *altura* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **Int**.

Essa função irá calcular a altura do nó junto com seus subnós, realizando assim também, o cálculo da altura de uma árvore avl.

Caso se recebe o **Vazio** é retornado **0**.

Caso contrário, é retornado a soma de 1 com o maior valor das alturas dos filhos à esquerda e à esquerda.

### Função rebalance

    rebalance :: (Ord a, Eq a) => TreeAvl a -> TreeAvl a

A função *rebalance* irá receber um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Essa função irá chamar as funções de rotação caso necessário.

### Função remover

    remover :: (Ord a, Eq a) => a -> TreeAvl a -> TreeAvl a

A função *remover* irá receber um valor do tipo genérico **a** e um valor do tipo **TreeAvl** e retorna um valor do tipo **TreeAvl**.

Essa função ira remover o valor na arvore caso esse se encontre, se não irá devolver a árvore intacta.

### Função geraLista

    geraLista :: (Ord a, Eq a) => TreeAvl a -> [a]

A função *geraLista* recebe um valor do tipo **TreeAvl** e retorna uma lista do tipo genérico **a**.

Essa função irá retornar os valores contidos na árvore AVL em formato de lista, partindo do valor que está mais à esquerda até o valor mais à direita, ou seja, irá retornar os valores em ordem de grandeza.

Caso se receba um **Vazio**, é retornado uma lista vazia.

Caso contrário, é retornado a lista gerada pelo valor do nó atual com as concatenações das listas geradas recursivamente dos filhos à esquerda e a direita.

### Função menorValor

    menorValor :: (Ord a, Eq a) => TreeAvl a -> a

A função *menorValor* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo genérico **a**.

Essa função irá retornar o menor valor contido em um nó, sendo utilizada a função *geraLista* para recolher os valores desse nó e então usado a função *head* para pegar pegar o primeiro valor dessa lista, que é o menor de todos os valores.

### Função geraAVL

    geraAVL :: (Ord a, Eq a) => TreeAvl a -> [a] -> TreeAvl a

A função *geraAVL* recebe um valor do tipo **TreeAvl** e uma lista do tipo genérico **a**.

Essa função irá inserir na árvore os valores contidos na lista.

Caso se receba uma lista vazia, é retornada a árvore sem modificação.

Caso contrário, é chamada a função inserir e é passado o primeiro elemento da lista com a arvore gerada recursivamente da árvore recebida com o restante da lista.

### Função removerAVL

    removerAVL :: (Ord a, Eq a) => TreeAvl a -> [a] -> TreeAvl a

A função *removerAVL* recebe um valor do tipo **TreeAvl** e uma lista do tipo genérico **a**.

Essa função irá remover da árvore os valores contidos na lista, desde que estes estejam na árvore.

Caso se receba uma lista vazia, é retornada a árvore sem modificação.

Caso contrário, é chamada a função remover e é passado o primeiro elemento da lista com a arvore gerada recursivamente da árvore recebida com o restante da lista.

## 3 - Manipulando a árvore AVL

Para criar uma árvore, use a seguinte sintaxe no ghci:

    Prelude> arvore = geraAVL Vazio [--Conteúdo da lista--]

Para imprimir o conteúdo da árvore, use a seguinte sintaxe no ghci:

    Prelude> print arvore

Para verificar se um valor está na arvore, use a seguinte sintaxe no ghci:

    Prelude> busca valor arvore

Para remover o conteúdo da lista, use a seguinte sintaxe no ghci:

    Prelude> removerAVL arvore [--Conteúdo da lista--] 


## 4 - Exemplos do uso

Inserindo os valores (4, 6, 35, 2, 8) em uma nova árvore:

    Prelude> arvore = geraAVL Vazio [4,6,35,2,8]
    Prelude> print arvore
    No (No (No Vazio 2 Vazio) 4 (No Vazio 6 Vazio)) 8 (No Vazio 35 Vazio)
    
![imagem_de_arvore](https://github.com/moukinchung/AVL_Tree_Haskell/blob/master/arvore.png)
###### Representação ilustrativa de arvore

Inserindo os valores (0, 8, 100, -95) em arvore


Inserindo os valores ("Vanessa","Raquel", "Debora", "Maria", "Jefferson") em uma nova árvore:

    Prelude> nomes = geraAVL Vazio ["Vanessa","Raquel", "Debora", "Maria", "Jefferson"]
    Prelude> print nomes
    No (No Vazio "Debora" Vazio) "Jefferson" (No (No Vazio "Maria" Vazio) "Raquel" (No Vazio "Vanessa" Vazio))

![imagem_da_arvore_nomes](https://github.com/moukinchung/AVL_Tree_Haskell/blob/master/nomes.png)
###### Representação ilustrativa de nomes
