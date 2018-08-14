# **Árvore AVL - Implementação em Haskell**

[**1 - Conceitualização**](https://github.com/moukinchung/AVL_Tree_Haskell#1---conceitualiza%C3%A7%C3%A3o)

[**2 - A implementação em Haskell**](https://github.com/moukinchung/AVL_Tree_Haskell#2---a-implementa%C3%A7%C3%A3o-em-haskell)

  [**- O tipo**]
  
  [**- Função busca**]
  
  [**- Função valor**]
  
  [**- Função esquerda**]
  
  [**- Função direita**]
  
  [**- Função rotacaoLL**]
  
  [**- Função rotacaoRR**]
  
  [**- Função rotacaoLR**]
  
  [**- Função rotacaoRL**]
  
  [**- Função inserir**]
  
  
[**3 - Manipulando a árvore AVL**](https://github.com/moukinchung/AVL_Tree_Haskell#3---manipulando-a-%C3%A1rvore-avl)

[**4 - Exemplos do uso**](https://github.com/moukinchung/AVL_Tree_Haskell#4---exemplos-do-uso)

## 1 - Conceitualização

A Árvore Adelson-Velskii e Landis (AVL) é uma árvore de busca binária balanceada que minimiza o número de comparações efetuadas no pior caso para uma busca nessa árvore.

Para isso é necessário haver um mecanismo para que seja mantida essa propriedade na árvore a cada inserção ou remoção.

Nessa estrutura de dado, cada elemento da árvore é considerado um nó, e a partir desse nó, haverá dois filhos, que poderão ser um outro nó, ou simplesmente um elemento vazio.

Uma árvore é considerada balanceada se a partir do nó principal, as alturas dos seus filhos diferem em módulo de até uma unidade, sendo que essa propriedade também é válida para cada sub-árvore.

## 2 - A implementação em Haskell

### O tipo

Para a implementação na linguagem Haskell, foi criado um novo tipo de dado para representar o nó do elemento com seus dois filhos, além de também poder representar o nó vazio:

    data TreeAvl a = Vazio | No (TreeAvl a) a (TreeAvl a)
                     deriving (Show, Eq, Ord)

Como foi definido com o tipo genérico ‘**a**’, é possível usar essa implementação tanto para tipos numéricos (Int, Integer, Float, Double) ou do tipo caractere (Char, String), assim como do tipo lista ou tupla.

### Função busca

    busca :: (Ord a, Eq a) => a -> TreeAvl a -> Bool

A função *busca* recebe um valor do tipo genérico **a** e um valor do tipo **TreeAvl**, e retorna um **bool**.

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

    altura :: (Ord a, Eq a) => TreeAvl a -> Int

A função *altura* recebe um valor do tipo **TreeAvl** e retorna um valor do tipo **Int**.

Essa função irá calcular a altura do nó junto com seus subnós, realizando assim também, o cálculo da altura de uma árvore AVL.

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

Essa função irá retornar o menor valor contido em um nó, sendo utilizada a função *geraLista* para recolher os valores desse nó e então usado a função *head* para pegar o primeiro valor dessa lista, que é o menor de todos os valores.

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

Para usar a implementação, entre dentro da pasta principal e abra o Terminal do Linux ou o cmd/Windows PowerShell no Windows e utilize o seguinte comando:
    
    stack ghci src/Main.hs

Para criar uma árvore, use o seguinte comando no ghci:

    Main> arvore = geraAVL Vazio [--Conteúdo da lista--]

Para imprimir o conteúdo de arvore, use o seguinte comando no ghci:

    Main> print arvore

Para verificar se um valor está em arvore, use o seguinte comando no ghci:

    Main> busca valor arvore

Para remover o conteúdo de arvore, use o seguinte comando no ghci:

    Main> removerAVL arvore [--Conteúdo da lista--] 


## 4 - Exemplos do uso

- Inserindo os valores (4, 6, 35, 2, 8) em uma nova árvore:

        Main> arvore = geraAVL Vazio [4,6,35,2,8]
        Main> print arvore
        No (No (No Vazio 2 Vazio) 4 (No Vazio 6 Vazio)) 8 (No Vazio 35 Vazio)
    
    <figure>
        <img src='https://github.com/moukinchung/AVL_Tree_Haskell/blob/master/arvore.png' alt='missing' />
        <figcaption>Representação ilustrativa de arvore</figcaption>
    </figure>

- Inserindo os valores (0, 8, 45, 100, -95) a partir de arvore:

        Main> arvore1 = geraAVL arvore [0, 8, 45, 100, -95]
        Main> print arvore1
        No (No (No Vazio (-95) Vazio) 0 (No Vazio 2 Vazio)) 4 (No (No Vazio 6 Vazio) 8 (No (No Vazio 35 Vazio) 45 (No Vazio 100 Vazio)))

    <figure>
        <img src='https://github.com/moukinchung/AVL_Tree_Haskell/blob/master/arvore1.png' alt='missing' />
        <figcaption>Representação ilustrativa de arvore1</figcaption>
    </figure>
    
- Inserindo os valores ("Maria", "Jefferson", "Vanessa", "Raquel", "Debora") em uma nova árvore nomes:

        Main> nomes = geraAVL Vazio ["Maria", "Jefferson", "Vanessa", "Raquel", "Debora"]
        Main> print nomes
        No (No (No Vazio "Debora" Vazio) "Jefferson" (No Vazio "Maria" Vazio)) "Raquel" (No Vazio "Vanessa" Vazio)

    <figure>
        <img src='https://github.com/moukinchung/AVL_Tree_Haskell/blob/master/nomes.png' alt='missing' />
        <figcaption>Representação ilustrativa de nomes</figcaption>
    </figure>

- Buscando os valores "Raquel" e "Mariana" em nomes:
    
        Main> busca "Raquel" nomes
        True
        Main> busca "Mariana" nomes
        False

- Removendo os valores ("Raquel", "Jefferson") de nomes:
        
        Main> nomes1 = removerAVL nomes ["Raquel", "Jefferson"]
        Main> print nomes1
        No (No Vazio "Debora" Vazio) "Maria" (No Vazio "Vanessa" Vazio)
        
     <figure>
        <img src='https://github.com/moukinchung/AVL_Tree_Haskell/blob/master/nomes1.png' alt='missing' />
        <figcaption>Representação ilustrativa de nomes1</figcaption>
    </figure>
