eseb2002_path = "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2006_path = "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2006/02489.sav"

eseb2002 = read.spss(eseb2002_path, to.data.frame=TRUE)
eseb2006 = read.spss(eseb2006_path, to.data.frame=TRUE)

satisfacao_democratica_2002 = as.data.frame(table(eseb2002$p19, exclude=NULL))
satisfacao_democratica_2006 = as.data.frame(table(eseb2006$eseb22, exclude=NULL))
