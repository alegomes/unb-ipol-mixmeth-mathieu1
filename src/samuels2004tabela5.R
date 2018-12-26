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

source('samuels2004.R')

model <- logit()

# Valores medios de cada variavel
data_means <- get_means()
opiniao_media <- data_means['opiniao_sobre_lula'][1,1]
ideologia_media <- data_means['ideologia_esquerda_direita'][1,1]

# Valores de cada variavel X que incorre na maior probabilidade de petismo
mais_provaveis <- get_mais_provaveis()

# Probabilidade de Y considerando todas os X's medios 
# "(…) primeiro, fixando todas as variáveis em seus valores médios e obtendo a 
# predição da linha-base."
means <- test_probability_of_means(model)

# "Depois, obtemos a probabilidade de ser petista quando todas as variáveis da
# Tabela 4 são colocadas em seus valores de “maior probabilidade de ser
# petista".
d <- mais_provaveis
d[1,'opiniao_sobre_lula'] <- opiniao_media
d[1,'ideologia_esquerda_direita'] <- ideologia_media
opiniao_media_ideologia_media_outras_maisprovavel <- predict(model, as.data.frame(d), type="response")

d <- mais_provaveis
d[1,'opiniao_sobre_lula'] <- opiniao_media
opiniao_media_outras_maisprovavel <- predict(model, as.data.frame(d), type="response")

d <- mais_provaveis
d[1,'ideologia_esquerda_direita'] <- ideologia_media
ideologia_media_outras_maisprovavel <- predict(model, as.data.frame(d), type="response")

d <- mais_provaveis
todas_maisprovavel <- predict(model, as.data.frame(d), type="response")

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
             digits = 5,
             split.table = Inf, 
             caption = "TABELA 5. Samuels (2004)", 
             plain.ascii = TRUE, 
             justify = 'center')

  # +-------------------------------+---------+
  # |                               | Petismo |
  # +===============================+=========+
  # |  Todas as variaveis em valor  | 0.19427 |
  # |      medio (linha base)       |         |
  # +-------------------------------+---------+
  # |     Opiniao sobre Lula e      | 0.99945 |
  # |        posicionamento         |         |
  # |  esquerda-direita na media,   |         |
  # | todas as outruas variaveis no |         |
  # |    cenario maiis provavel     |         |
  # +-------------------------------+---------+
  # |  Opiniao sobre Lula na media  | 0.99955 |
  # | e todas as outras no cenario  |         |
  # |         mais provavel         |         |
  # +-------------------------------+---------+
  # |        Posicionamento         | 0.99994 |
  # |  esquerda-direita na media e  |         |
  # |  todas as outras no cenario   |         |
  # |         mais provavel         |         |
  # +-------------------------------+---------+
  # |  Todas variaveis no cenario   | 0.99995 |
  # |         mais provavel         |         |
  # +-------------------------------+---------+
  # 
  # Table: TABELA 5. Samuels (2004)
