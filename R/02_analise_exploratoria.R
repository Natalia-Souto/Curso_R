# Script de análise exploratória de dados contidos no R
# carregandi dados
data("anscombe")

# dimensao dos dados, N de linhas e N de colunas
dim(anscombe)

# seis primeiras linhas dos dados
head(anscombe)

# classe do objeto
class(anscombe)

# estrutura do objeto
str(anscombe)

# fazer as médias das colunas
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

# Fazer as médias das colunas mas agora com apenas uma linha de comando usando a função apply, aplica uma funcao a todas as linhas de um objeto
## o mesmo calculo, agora apenas em 1 linha de comando
### media de todos os vetores x
apply(anscombe[,1:4], 2, mean)

## media de todos os vetores y
apply(anscombe[, 5:8], 2, mean)

# Descrição estatística dos dados
## variância dos dados
### aplica a funcao var a todas as linhas do objeto
apply(anscombe, 2, var)

## Ententendo a correlação e coeficiente de regressão dos conjuntos x e y.
### correlacao
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

## coeficiente de regressão
### primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)

### vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)

### agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

# Os valores parecem difentes. Mas quão diferentes?
## # funcao par para definir as configuracoes da janela grafica entre em ?par
?par


par(mfrow=c(2, 2),
    las=1,
    bty = "l")

#plot das variaveis
plot(anscombe$y1 ~ anscombe$x1)
abline(mlist[[1]])

plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])

plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])

plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

# retorna a janela grafica para o padrao de 1 linha e 1 coluna
par(mfrow=c(1, 1))
