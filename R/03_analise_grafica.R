# Script analise grafica dados Iris
head(iris)
summary(iris)

#Quantas informações por espécie?
table(iris$Species)

# Qual a média das variáveis por espécie?
## As duas funções são semelhantes, o que muda são os argumentos e o formato de saída de cada uma delas.
### media do comprimento de sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)

### a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)

### ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)

###para outras variaves
aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)

#calcular o desvio padrão das variáveis.
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

# solução de como calular a média por espécie de todas as variáveis
## criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)

## definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}

# Estatísticas descritivas
## Medidas de tendência central
### media
vars <- iris[, -5]
apply(vars, 2, mean)

### Mediana: 50º quantil, de forma que divide os dados em duas metades
apply(vars, 2, median)

### Moda: valor mais frequente na amostra
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

# Medidas de dispersão
## Variância: desvio da média
apply(vars, 2, var)

## Desvio padrão: raiz quadrada da variância
sd01 <- apply(vars, 2, sd)

### outra forma
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01 == sd02

# Coeficiente de variação: medida relativa de desvio padrão
## Não existe no R base uma função para calcular o coeficiente de variação. Isto não é um problema. Vamos formalmente criar nossa primeira função de R. Para iss, usamos a função function
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)

# Quantis ou percentis
## sumario de 5 numeros
apply(vars, 2, quantile)
## 5%, 50% e 95%
apply(vars, 2, quantile, probs = c(0.05, 0.5, 0.95))

# Intervalo (range)
## O intervalo é a diferença entre o maior e o menor valor de determinada variável.
### a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
### aplicando a funcao diff ao resultado do range, temos o valor desejado
### uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
### nunca nomeie um objeto com um nome já existente
my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)

# Intervalo interquartil (IIQ)
## O IIQ é a diferença entre o quartil superior (75%) e o quartil inferior (25%).
apply(vars, 2, IQR)

# Correlação
## Uma matriz de correlação é uma tabela com os valores de correlação entre cada uma das variáveis par a par.
## As variáveis podem ser correlacionadas positivamentes (valores positivos) ou negativamente (valores negativos)
cor(vars)

# Métodos gráficos
# Gráfico de barras
## Um gráfico de barras mostra a frequência de de observações em uma dada classe.
barplot(table(iris$Species))

# Gráfico de barras
## mostra a frequência de de observações em uma dada classe.
## Cada barra representa um intervalo de valores.
par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)

par(mfrow=c(1, 1))

# ver o efeito do número de intervalos no histograma com o argumento breaks
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)

par(mfrow=c(1, 1))

# Curva de densidade
## A curva de densidade mostra a probabilidade de observar determinado valor
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)

par(mfrow=c(1, 1))

# podemos ver a curva de densidade a usando a função por meio do plot da função density
par(mfrow=c(1, 2))

## plot da curva de densidade
plot(density(iris$Sepal.Width))

## plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue")
### note que agora estamos usando a funcao o comando add=TRUE

par(mfrow=c(1, 1))

# Box-plot ou box-whisker plot
## fazer os box-plots das variáveis contidas no objeto iris
##começar com as variáveis gerais.
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

# olhando para os valores por espécie
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

# Para identificar os outliers usar a própria função boxplot
boxplot(iris$Sepal.Width)

my_boxplot <- boxplot(iris$Sepal.Width, plot = FALSE)
my_boxplot

## o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista

outliers <- my_boxplot$out

##qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)

## vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

# É razoável assumir que cada espécie tenha um padrão morfométrico distinto de modo que poderíamos identificar outliers de maneira espécie específica
boxplot(Sepal.Width ~ Species, data = iris)

my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2

## o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out

## neste caso, queremos apenas os outliers da especie setosa
## vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species == "setosa",
     c("Sepal.Width", "Species")]


# Entendendo a distribuição dos dados
## olhar para os dados morfométricos das espécies de Iris e comparar com uma distribuição normal.
##No R, isto pode ser feito de forma visual com as funções qqnorm e qqline.
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"],
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])

par(mfrow=c(1,1))

# Relação entre variáveis
## Uma função no R que nos ajuda a explorar a relação entre muitas variáveis é a pairs.
##O resultado é uma matriz com variáveis em linhas e colunas o gráfico que vemos é o gráfico de dispersão para cada par de variáveis.
pairs(vars)
