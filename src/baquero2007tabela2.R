eseb2002_path = "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2006_path = "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2006/02489.sav"

eseb2002 = read.spss(eseb2002_path, to.data.frame=TRUE)
eseb2006 = read.spss(eseb2006_path, to.data.frame=TRUE)

opiniaoDemocracia2002 = as.data.frame(summary(eseb2002$p90))
opiniaoDemocracia2006 = as.data.frame(summary(eseb2006$eseb23))

col_header = c('A democracia é sempre melhor que outra forma de governo',
               'Em algumas situações a ditadura é melhor que a democracia',
               'Tanto faz/nenhuma das duas é melhor',
               'NS',
               'Total')
  
eseb2002$p90perc <- eseb2002$p90 / sum(eseb2002$p90)
