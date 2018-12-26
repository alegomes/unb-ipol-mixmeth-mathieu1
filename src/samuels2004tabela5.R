############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 5 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)

# (…) precisamos comparar o impacto relativo destas duas variáveis (opinião
# sobre Lula e posicionamento ideológico esquerda-direita) (antes de continuar a
# discutir as outras variáveis estatisticamente significativas) (…). O modo de
# fazer essa comparação dá-se, primeiro, fixando todas as variáveis em seus
# valores médios e obtendo a predição da linha-base. Depois, obtemos a
# probabilidade de ser petista quando todas as variáveis da Tabela 4 são
# colocadas em seus valores de “maior probabilidade de ser petista”. Ou seja,
# podemos mandar o CLARIFY fixar “avaliação retrospectiva” em seu valor mínimo
# (isto é, anti-FHC), a “opinião sobre Lula” em seu valor máximo, esquerdismo em
# seu valor mínimo (isto é “mais esquerdista”) para todos os brasileiros e
# depois pedir ao programa para predizer a probabilidade de um brasileiro com
# aquelas características ser petista. Em seguida, podemos mudar os valores de
# qualquer das variáveis a fim de medir o impacto relativo de mudar o valor de
# uma determinada variável sobre a predição. A Tabela 5 apresenta vários
# cenários. (Pg 235)

means <- 0.138
opiniao_media_ideologia_media_outras_maisprovavel <- 0.785
opiniao_media_outras_maisprovavel <- 0.892
ideologia_media_outras_maisprovavel <- 0.989
todas_maisprovavel <- 0.996

tabela5 <- data.frame(Petismo=c(means, 
                                opiniao_media_ideologia_media_outras_maisprovavel,
                                opiniao_media_outras_maisprovavel,
                                ideologia_media_outras_maisprovavel,
                                todas_maisprovavel), 
                      row.names=c('Todas as variaveis em valor medio (linha base)',
                                  'Opiniao sobre Lula e posicionamento esquerda-direita na media, todas as outruas variaveis no cenario maiis provavel',
                                  'Opiniao sobre Lula na media e todas as outras no cenario mais provavel',
                                  'Posicionamento esquerda-direita na media e todas as outras no cenario mais provavel',
                                  'Todas variaveis no cenario mais provavel'))


pandoc.table(tabela5, 
             style = 'grid', 
             # round = c(1:4), 
             digits = 3,
             split.table = Inf, 
             caption = "TABELA 4. Samuels (2004)", 
             plain.ascii = TRUE, 
             justify = 'center')
