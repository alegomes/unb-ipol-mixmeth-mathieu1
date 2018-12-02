############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da GRAFICO 2 do artigo Baquero (2007)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(foreign)
library(dplyr)
library(plotly)

eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2006_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2006/02489.sav"

# Abre a base
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)
eseb2006 <- read.spss(eseb2006_path, to.data.frame=TRUE)

# Cria vetor com dados de 2002 (somatorio das avaliacoes otimas e boas de cada instituicao)
dados2002 <- NULL
for (c in letters[1:10]) {
  i = as.data.frame(table(eseb2002[,paste('p131',c,sep='')]) %>% prop.table())
  dados2002 <- c(dados2002, round(sum(i[5:6,2])*100,digits=1))
}

# Cria vetor com dados de 2006 (somatorio das avaliacoes otimas e boas de cada instituicao)
dados2006 <- NULL
for (c in letters[1:10]) {
  i = as.data.frame(table(eseb2006[,paste('eseb52',c,sep='')]) %>% prop.table())
  dados2006 <- c(dados2006, round(sum(i[5:6,2])*100,digits=1))
}

instituicoes <- c('Igreja Catolica', 
                  'Governo Federal', 
                  'Policia',
                  'Justica',
                  'Grandes empresas',
                  'Partidos politicos',
                  'Congresso',
                  'Militares',
                  'Rede Globo',
                  'Outras emissoras de televisao')

dados <- data.frame(instituicoes, dados2002, dados2006)

xaxis <- list(categoryorder = "array",
              categoryarray = instituicoes,
              title='')

yaxis <- list(title = '')

margin <- list(autoexpand = TRUE,
               l = 100,
               r = 100,
               b = 100,
               t = 400,
               pad = 4)

plot_ly(dados, 
        x = ~instituicoes, 
        y = ~dados2002,  
        type = 'bar',
        name = '2002',
        marker = list(color = 'rgb(255,255,255)',
                      line = list(color = 'rgb(0,0,0)', 
                                  width = 1.5)),
        text = dados2002, 
        textposition = 'outside') %>%
        add_trace(y = ~dados2006, 
                  name = '2006', 
                  marker = list(color = 'rgb(0,0,0)'),
                  text = dados2006, 
                  textposition = 'outside') %>%
        layout(title = "GRAFICO 2\nAvaliacao Institucional positiva (%)") %>% 
        layout(xaxis = xaxis) %>% 
        layout(yaxis = yaxis, barmode = 'group') 
