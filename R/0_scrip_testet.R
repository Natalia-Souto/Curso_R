# script para ler a tabela limpa
# os dados originais estao em um csv

?read.table
help(read.table)

# para nomear a tabela
# indica que a tabela que vai usar esta em determinada pasta
Tabela <- read.table("Data/dados_editados2.csv")

# para ler a tabela no terminal
read.csv2("Data/dados_editados2.csv")

