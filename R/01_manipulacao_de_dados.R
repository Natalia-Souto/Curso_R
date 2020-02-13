# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#-----------------------------------------------------------#

# baixando o pacote necessario #####
library(tidyr)

# para entender a ferramenta #####
?list.files


# carregando as pastas para dentro do R ######
files.path <- list.files(path = "Data/cestes",
                        pattern = ".csv" ,
                        full.names = TRUE)

# renomeando as pastas ######
comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])

# explorando as tabelas ######
head (comm)
dim (comm)
summary (comm)

head (coord)
dim (coord)
summary (coord)

head (envir)
dim (envir)
summary(envir)

head (splist)
dim (splist)
summary(splist)

head (traits)
dim(traits)
summary(traits)

# sumario dos dados
##Temos dados de quantas espécies? Podemos simplesmente contar o número de linhas do objeto splist.
nrow(splist)

##Quantas áreas amostradas? Podemos contar o número de linhas dos objetos comm ou envir
nrow (comm)
nrow(envir)

##Quantas variáveis ambientais?
### todas as variáveis exceto a primeira coluna com o id
names(envir) [-1]

#### contando quantas variáveis
length(names(envir) [-1])

#Qual a riqueza de cada área?
## ransformar a nossa matriz que possui dados de abundância em uma matriz de presença e ausência. ####

comm.pa <- comm[, -1] > 0

# nomear as linhas das planilhas com o id dos sites
head(comm.pa)
row.names(comm.pa) <- envir$Sites

# Vamos calcular a riqueza da área 1, por exemplo, somando a primeira linha do novo objeto comm.pa.
sum(comm.pa[1, ])

# Como podemos fazer a soma de forma automatizada para as 97 áreas? Podemos usar a função apply. Essa função aplica uma função às linhas ou colunas de um objeto (do tipo data.frame ou matrix)
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)


summary(rich)

#Juntando diferentes tabelas por meio de identificadores comuns
envir$Sites

summary(envir$Sites)

#Transformando tipos de variáveis
## se checarmos a classe desse vetor, veremos que é numerica
class(envir$Sites)

## queremos que seja uma variável categórica. Para isso, convertemos em fator
as.factor(envir$Sites)

## se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
envir$Sites <- as.factor(envir$Sites)

coord$Sites <- as.factor(envir$Sites)

#Juntando coord e envir
envir.coord <- merge(x = envir,
                     y = coord,
                     by = "Sites")

#  Quantas colunas deveríamos ter ao final? Quais colunas foram adicionadas?
dim(envir)
dim(coord)
dim(envir.coord)
head(envir.coord)

# Transformando uma matrix espécie vs. área em uma tabela de dados

### vetor contendo todos os Sites
Sites <- envir$Sites

length(Sites)

# vetor número de espécies
n.sp <- nrow(splist)

n.sp

# criando tabela com cada especie em cada area especies em linhas
comm.df <- tidyr::gather(comm[, -1])

# checar o cabeçalho e as dimensões do objeto.
dim(comm.df)
head(comm.df)

# alterar o nome das colunas de comm.df. Para isso, usaremos a função colnames()

## nomes atuais
colnames(comm.df)

## modificando os nomes das colunas
colnames(comm.df) <- c("TaxCode", "Abundance")

## checando os novos nomes
colnames(comm.df)

#Queremos agora adicionar a coluna Sites ao novo objeto
## primeiro criamos a sequência
seq.site <- rep(Sites, times =n.sp)

# checando a dimensão
length(seq.site)

# adicionando ao objeto comm.df
comm.df$Sites <- seq.site

# checando como ficou
head(comm.df)

# Juntando todas as variáveis à comm.df
## Tabela comm.df e splist
###vamos adicionar as informações das espécies contidas em splist à comm.df usando a coluna TaxCode.
comm.sp <- merge(comm.df, splist, by = "TaxCode")

head(comm.sp)

## Tabela comm.sp e traits
### adicionamos os dados de atributos das espécies à tabela de comunidade
#### Antes de fazer a junção, precisamos mudar o nome para bater com o nome da coluna em comm.sp que é TaxCode
names(traits)

#### Renomeando primeiro elento
colnames(traits)[1] <- "TaxCode"

comm.traits <- merge(comm.sp, traits, by = "TaxCode")

head(comm.traits)

# Tabela comm.traits e envir.coord
## juntamos as variáveis ambientais (que aqui já contém as coordenadas) à tabela geral da comunidade por meio da coluna Sites
comm.total <- merge(comm.traits, envir.coord, by = "Sites")

#  finalizamos nossa rotina de manipulação de dados exportando a planilha final modificada
write.csv(x = comm.total,
          file = "data/01_data_format_combined.csv",
          row.names = FALSE)

