############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 4 do artigo Baquero (2007)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(foreign)

eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2006_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2006/02489.sav"

# Abre a base
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)
eseb2006 <- read.spss(eseb2006_path, to.data.frame=TRUE)

# Questoes 31 e 33 da ESEB 2002
eseb2002q31 <- eseb2002$p31
eseb2002q33 <- eseb2002$p33

# Questoes 7 e 9 da ESEB 2006, considerando apenas as respostas Sim e Nao.
eseb2006q7 <- eseb2006$eseb7[eseb2006$eseb7 == levels(eseb2006$eseb7)[1] | eseb2006$eseb7 == levels(eseb2006$eseb7)[2]]
eseb2006q9 <- eseb2006$eseb9[eseb2006$eseb9 == levels(eseb2006$eseb9)[1] | eseb2006$eseb9 == levels(eseb2006$eseb9)[2]]

# Consolidacao dos valores absolutos
representacao <- rbind(
                    c(tapply(eseb2002q31, eseb2002q31, length)[c(levels(eseb2002q31)[2], levels(eseb2002q31)[1])], 
                      tapply(eseb2006q7, eseb2006q7, length)[c(levels(eseb2006q7)[2], levels(eseb2006q7)[1])]),
                    c(tapply(eseb2002q33, eseb2002q33, length)[c(levels(eseb2002q31)[2], levels(eseb2002q31)[1])], 
                      tapply(eseb2006q9, eseb2006q9, length)[c(levels(eseb2006q9)[2], levels(eseb2006q9)[1])]))

n2002q31 <- sum(representacao[1,1:2])
n2002q33 <- sum(representacao[2,1:2])
n2006q7 <- sum(representacao[1,3:4])
n2006q9 <- sum(representacao[2,3:4])
  
# Calculo dos valores relativos                        
representacao <-  rbind(
                    c((representacao[1,1] / sum(representacao[1,1:2])) * 100,
                      (representacao[1,2] / sum(representacao[1,1:2])) * 100,
                      (representacao[1,3] / sum(representacao[1,3:4])) * 100,
                      (representacao[1,4] / sum(representacao[1,3:4])) * 100),
                    c((representacao[2,1] / sum(representacao[2,1:2])) * 100,
                      (representacao[2,2] / sum(representacao[2,1:2])) * 100,
                      (representacao[2,3] / sum(representacao[2,3:4])) * 100,
                      (representacao[2,4] / sum(representacao[2,3:4])) * 100))

row.names(representacao) <- c('Algum partido representa sua maneira de pensar?',
                              'Algum candidato a presidente desta eleicao representa sua maneira de pensar?')

colnames(representacao) <- c('2002 Sim', '2002 Nao', '2006 Sim', '2006 Nao')

pandoc.table(representacao, 
             style = 'grid', 
             round = c(1:4), 
             digits = 3,
             split.table = Inf, 
             caption = sprintf("TABELA 4. Baquero (2007)\n\tn2002p31=%d, n2002p33=%d, n2006p7=%d, n2006p9=%d", n2002q31, n2002q33, n2006q7, n2006q9), 
             plain.ascii = TRUE, 
             justify = 'center')

