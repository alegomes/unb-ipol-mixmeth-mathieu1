############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da GRAFICO 1 do artigo Baquero (2007)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(foreign)
library(dplyr)
library(plotly)
library(ggplot2)


eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2006_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2006/02489.sav"

# Abre a base
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)
eseb2006 <- read.spss(eseb2006_path, to.data.frame=TRUE)

eseb2002q89 <- as.data.frame(with(eseb2002, table(p89)) %>% prop.table())
eseb2002q87 <- as.data.frame(with(eseb2002, table(p87)) %>% prop.table())
eseb2002q21 <- as.data.frame(with(eseb2002, table(p21)) %>% prop.table())

eseb2006q55 <- as.data.frame(with(eseb2006, table(eseb55)) %>% prop.table())
eseb2006q54 <- as.data.frame(with(eseb2006, table(eseb54)) %>% prop.table())
eseb2006q5  <- eseb2006[which(eseb2006$eseb5 %in% levels(eseb2006$eseb5)[1:5]),] # Descarta as respostas do tipo 'Nao sabe' ou 'Nao respondeu'
eseb2006q5  <- as.data.frame(with(eseb2006q5,table(eseb5)) %>% prop.table())

# Eleicoes presidenciais...

rownames(eseb2006q55) <- c(3,2,1,4,5)
eseb2006q55 <- eseb2006q55[order(as.numeric(row.names(eseb2006q55))),]

eleicoes <- merge(eseb2002q89, eseb2006q55, by=0, all=TRUE)[1:3,c(3,5)]
row.names(eleicoes) <- c('Eleicoes presidenciais nao ajudam a melhorar a vida da populacao',
                         'Eleicoes presidenciais ajudam um pouco a melhorar a vida da populacao',
                         'Eleicoes presidenciais ajudam muito a melhorar a vida da populacao')
colnames(eleicoes) <- c(2002,2006)

# Eh melhor um presidente da republica que...

presidente <- merge(eseb2002q87, eseb2006q54, by=0, all=TRUE)[1:2,c(3,5)]
row.names(presidente) <- c('Eh melhor um presidente que seja identificado com um partido politico', 
                           'Eh melhor um presidente que nao de importancia para os partidos')
colnames(presidente) <- c(2002,2006)

# O voto...

rownames(eseb2006q5) <- c(5,4,3,2,1,6,7)
eseb2006q5 <- eseb2006q5[order(as.numeric(row.names(eseb2006q5))),]
votos <- merge(eseb2002q21, eseb2006q5, by=0, all=TRUE)[1:5,c(3,5)]
row.names(votos) <- c('O voto nao influencia nada no que acontece no Brasil',
                      'O voto nao influencia muito no que acontece no Brasil',
                      'O voto eh indiferente sobre o que acontece no Brasil',
                      'O voto influencia pouco no que acontece no Brasil',
                      'O voto influencia muito no que acontece no Brasil')
colnames(votos) <- c(2002,2006)


# Avaliacao dos procedimentos poliarquicos - Dimensao negativa 

procedimentos_poliarquicos <- data.frame(rbind(c('2002'=sum(eleicoes[1:2,1]),'2006'=sum(eleicoes[1:2,2])),
                                               c('2002'=presidente[2,1],'2006'=presidente[2,2]),
                                               c('2002'=votos[4,1],'2006'=votos[4,2])),
                                           row.names=c('Eleicoes presidenciais nao ajudam ou ajudam pouco a populacao',
                                           'E melhor um presidente da republica que nao de importancia aos partidos',
                                           'O voto influencia pouco no que acontece no Brasil'))


# Grafico

xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))

yaxis <- list(title = "",
              showgrid = TRUE,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE)

margin <- list(autoexpand = TRUE,
               l = 100,
               r = 100,
               b = 100,
               t = 100,
               pad = 4)

eleicoes_1 <- list(
  xref = 'paper',
  yref = 'y',
  x = 0.05,
  y = procedimentos_poliarquicos[1,1],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste(round(procedimentos_poliarquicos[1,1]*100), '%'),
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
              showarrow = FALSE)

eleicoes_2 <- list(
  xref = 'paper',
  yref = 'y',
  x = 1.05,
  y = procedimentos_poliarquicos[1,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste(round(procedimentos_poliarquicos[1,2]*100), '%'),
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

presidente_1 <- list(
  xref = 'paper',
  yref = 'y',
  x = 0.05,
  y = procedimentos_poliarquicos[2,1],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste(round(procedimentos_poliarquicos[2,1]*100), '%'),
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

presidente_2 <- list(
  xref = 'paper',
  yref = 'y',
  x = 1.05,
  y = procedimentos_poliarquicos[2,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste(round(procedimentos_poliarquicos[2,2]*100), '%'),
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

voto_1 <- list(
  xref = 'paper',
  yref = 'y',
  x = 0.05,
  y = procedimentos_poliarquicos[3,1],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste(round(procedimentos_poliarquicos[3,1]*100), '%'),
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

voto_2 <- list(
  xref = 'paper',
  yref = 'y',
  x = 1.05,
  y = procedimentos_poliarquicos[3,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste(round(procedimentos_poliarquicos[3,2]*100), '%'),
  font = list(family = 'Arial',
              size = 16,
              color = 'rgba(67,67,67,1)'),
  showarrow = FALSE)

plot_ly(procedimentos_poliarquicos, 
          x = colnames(procedimentos_poliarquicos), 
          y = as.numeric(procedimentos_poliarquicos[1,]), 
          name = row.names(procedimentos_poliarquicos[1,]), 
          type = 'scatter', 
          mode = 'lines+markers') %>%
        add_trace(
          y = as.numeric(procedimentos_poliarquicos[2,]), 
          name = row.names(procedimentos_poliarquicos[2,]), 
          mode = 'lines+markers') %>%
        add_trace(
          y = as.numeric(procedimentos_poliarquicos[3,]), 
          name = row.names(procedimentos_poliarquicos[3,]), 
          mode = 'lines+markers') %>%
      layout(title = "Grafico 1 - Avaliacao dos procedimentos poliarquicos\nDimensao negativa (%)",
         #width = 1000, height = 500,
         xaxis = xaxis, yaxis = yaxis, margin = margin,
         autosize = TRUE,
         showlegend = TRUE,
         legend = list(x=1.1, y=0.9),
         annotations = eleicoes_1) %>%
         layout(annotations = eleicoes_2) %>%
         layout(annotations = presidente_1) %>%
         layout(annotations = presidente_2) %>%
         layout(annotations = voto_1) %>%
         layout(annotations = voto_2)
         


