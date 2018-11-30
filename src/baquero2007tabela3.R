############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 3 do artigo Baquero (2007)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(foreign)

eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2006_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2006/02489.sav"

# Abre a base
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)
eseb2006 <- read.spss(eseb2006_path, to.data.frame=TRUE)

# Busca as respostas as questoes 19 (2002) e 22 (2006)
satisfacao_democratica_2002 <- as.data.frame(table(eseb2002$p19, exclude=NULL))
satisfacao_democratica_2006 <- as.data.frame(table(eseb2006$eseb22, exclude=NULL))

# Renomeia e reordena a linhas dos dados de 2006 para coincidir com a codificacao usada em 2002
row.names(satisfacao_democratica_2006) <- c(5,4,3,2,1,77,99)
satisfacao_democratica_2006 <- satisfacao_democratica_2006[ order(as.numeric(row.names(satisfacao_democratica_2006))),]

# Recodifica os missing values de 2002, ja que nao tao estao especificados como em 2006
row.names(satisfacao_democratica_2002) <- c(1:6,999)

colnames(satisfacao_democratica_2002) <- c('Var2','v2002')
colnames(satisfacao_democratica_2006) <- c('Var2','v2006')

# Junta dados de 2002 com os de 2006 numa unico dataframe
satisfacao_democratica <- merge(satisfacao_democratica_2002, satisfacao_democratica_2006, by=0, all=TRUE)

rownames(satisfacao_democratica) <- c('Nada satisfeito', 
                                      'Pouco Satisfeito',
                                      'Nem satisfeito nem insatisfeito',
                                      'Satisfeito',
                                      'Muito satisfeito', 
                                      'Nao sabe o que e democracia',
                                      'NS',
                                      'NR',
                                      'NA')

# Calculo dos percentuais
satisfacao_democratica$p2002 <- (satisfacao_democratica$v2002 / sum(satisfacao_democratica$v2002, na.rm=TRUE)) * 100
satisfacao_democratica$p2006 <- (satisfacao_democratica$v2006 / sum(satisfacao_democratica$v2006, na.rm=TRUE)) * 100

# Remove colunas desnecessarias
satisfacao_democratica[,c(1:5)] <- NULL

# Adiciona total
satisfacao_democratica <- rbind(satisfacao_democratica, 
                                Total = c(sum(satisfacao_democratica[,1], na.rm=TRUE),
                                  sum(satisfacao_democratica[,2], na.rm=TRUE)))

#satisfacao_democratica <- rbind(satisfacao_democratica, 
#                                c('Total',colSums(satisfacao_democratica[,1:2])))


pandoc.table(satisfacao_democratica, 
             style = 'grid', 
             round = c(1:2), 
             digits = 3,
             split.table = Inf, 
             caption = "TABELA 3. Baquero (2007)", 
             plain.ascii = TRUE, 
             justify = c('center', 'right', 'right'))

