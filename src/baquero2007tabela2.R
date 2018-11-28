eseb2002_path = "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2006_path = "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2006/02489.sav"

eseb2002 = read.spss(eseb2002_path, to.data.frame=TRUE)
eseb2006 = read.spss(eseb2006_path, to.data.frame=TRUE)

opiniao_democracia_2002 = as.data.frame(table(eseb2002$p90, exclude=NULL))
opiniao_democracia_2006 = as.data.frame(table(eseb2006$eseb23, exclude=NULL))

resposta1_2002 = 'A democracia \351 sempre melhor que outra forma de governo'
resposta2_2002 = 'Em algumas situa\347\365es a ditadura \351 melhor do que a democracia'
resposta3_2002 = 'Tanto faz/ Nenhuma das duas \351 melhor'
resposta77_2002 = '<NA>'

resposta1_2006 = 'A democracia \351 sempre melhor que qualquer outra forma e gove'
resposta2_2006 = 'Em algumas situa\347\365es \351 melhor uma ditadura do que uma democr'
resposta3_2006 = 'Tanto faz / nenhuma das duas \351 melhor'
resposta77_2006 = 'N\343o sabe'

tabela2_header = c('A democracia é sempre melhor que outra forma de governo',
                   'Em algumas situações a ditadura é melhor que a democracia',
                   'Tanto faz/nenhuma das duas é melhor',
                   'NS',
                   'Total')

opiniao_democracia <- cbind(
                        rbind( opiniao_democracia_2002[which(opiniao_democracia_2002$Var1 == resposta1_2002),][2],
                               opiniao_democracia_2002[which(opiniao_democracia_2002$Var1 == resposta2_2002),][2],
                               opiniao_democracia_2002[which(opiniao_democracia_2002$Var1 == resposta3_2002),][2],
                               opiniao_democracia_2002[is.na(opiniao_democracia_2002$Var1),][2]),
                        rbind( opiniao_democracia_2006[which(opiniao_democracia_2006$Var1 == resposta1_2006),][2],
                               opiniao_democracia_2006[which(opiniao_democracia_2006$Var1 == resposta2_2006),][2],
                               opiniao_democracia_2006[which(opiniao_democracia_2006$Var1 == resposta3_2006),][2],
                               opiniao_democracia_2006[which(opiniao_democracia_2006$Var1 == resposta77_2006),][2]))
                        
                      

col_header = c('A democracia é sempre melhor que outra forma de governo',
               'Em algumas situações a ditadura é melhor que a democracia',
               'Tanto faz/nenhuma das duas é melhor',
               'NS',
               'Total')
  
eseb2002$p90perc <- eseb2002$p90 / sum(eseb2002$p90)
