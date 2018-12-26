############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 4 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)

# (…) utilizaremos um procedimento que permite comparações fáceis do impacto
# relativo de cada variável. Isto significa usar o módulo do software CLARIFY
# para o pacote estatístico STATA 8.0 (KING et al, 2000; TOMZ et al, 2003)7. O
# CLARIFY toma os coeficientes de regressão em modelos logísticos e calcula a
# probabilidade predita de obter qualquer dos resultados na variável dependente
# dado um conjunto de parâmetros para as variáveis independentes. Assim, o
# modelo de regressão inicial, por exemplo, poderia predizer que a probabilidade
# de um brasileiro ser petista é .20, significando que o modelo prediz que um em
# cada cinco brasileiros é petista8.
# 
# O CLARIFY não pode ser rodado através do SPSS (e nem no R!!!). É claro que se
# pode computar probabilidades preditas à mão, mas o procedimento pode se tornar
# muito árduo. Ver King et al (2000) sobre o uso do CLARIFY .
# https://gking.harvard.edu/files/clarify.pdf

# Na Tabela 4 apresentamos as mudanças em probabilidades preditas para as
# variáveis independentes que eram estatisticamente relevantes no modelo da
# Tabela 3. Inicialmente, todas as variáveis independentes foram colocadas em
# seu valor médio. A primeira linha da Tabela apresenta a linha-base da
# probabilidade predita de um brasileiro ser petista dados estes valores. Em
# essência, esta é a probabilidade de que um brasileiro com disposição “média”
# em relação a todas as variáveis do modelo seja petista. Os valores nas células
# representam então a mudança na probabilidade predita de ser um petista que
# resulta da mudança de valor apenas daquela determinada variável, independente
# de sua média para seu máximo, enquanto todas as outras são mantidas na média.
# Os valores nas células podem, portanto, variar de -1.00 a +1.00, e podem ser
# lidos como porcentagens.

source('samuels2004.R')

# Modelo a partir dos dados da pesquisa
model <- logit()

# Probabilidade de Y considerando todas os X's medios
means <- test_probability_of_means(model)

# Probabilidade de Y para cada X maximizado
test_opiniao_sobre_lula <- test_probability_of_opiniao_sobre_lula(model)
test_ideologia_esquerda_direita <- test_probability_of_ideologia_esquerda_direita(model)
test_participacao_nao_eleitoral <- test_probability_of_participacao_nao_eleitoral(model) 
test_escolaridade <- test_probability_of_escolaridade(model)
test_conhecimento_politico <- test_probability_of_conhecimento_politico(model)
test_avaliacao_governo_fhc <- test_probability_of_avaliacao_governo_fhc(model)
test_eficacia_participacao <- test_probability_of_eficacia_participacao(model)
test_participacao_eleitoral <- test_probability_of_participacao_eleitoral(model)

tabela4 <- data.frame(Petismo=c(means, 
                              test_opiniao_sobre_lula,
                              test_ideologia_esquerda_direita,
                              test_participacao_nao_eleitoral,
                              test_escolaridade,
                              test_conhecimento_politico,
                              test_avaliacao_governo_fhc,
                              test_eficacia_participacao,
                              test_participacao_eleitoral), 
                    row.names=c('Linha Base',
                                'Opiniao sobre Lula',
                                'Participacao esquerda-direita',
                                'Participacao nao eleitoral',
                                'Escolaridade',
                                'Conhecimento politico',
                                'Avaliacao retrospectiva',
                                'Eficacia do voto',
                                'Participacao eleitoral') )


pandoc.table(tabela4, 
             style = 'grid', 
             # round = c(1:4), 
             digits = 3,
             split.table = Inf, 
             caption = "TABELA 4. Samuels (2004)", 
             plain.ascii = TRUE, 
             justify = 'center')


#   +--------------------------------+---------+
#   |                                | Petismo |
#   +================================+=========+
#   |           Linha Base           |  0.976  |
#   +--------------------------------+---------+
#   |       Opiniao sobre Lula       |  0.975  |
#   +--------------------------------+---------+
#   |          Participacao          |  0.954  |
#   |        esquerda-direita        |         |
#   +--------------------------------+---------+
#   |   Participacao nao eleitoral   |  0.942  |
#   +--------------------------------+---------+
#   |          Escolaridade          |  0.964  |
#   +--------------------------------+---------+
#   |     Conhecimento politico      |  0.969  |
#   +--------------------------------+---------+
#   |    Avaliacao retrospectiva     |  0.993  |
#   +--------------------------------+---------+
#   |        Eficacia do voto        |  0.976  |
#   +--------------------------------+---------+
#   |     Participacao eleitoral     |  0.967  |
#   +--------------------------------+---------+
#   
#   Table: TABELA 4. Samuels (2004)