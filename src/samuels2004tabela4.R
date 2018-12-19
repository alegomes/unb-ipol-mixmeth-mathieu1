############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 4 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)


# (…) utilizaremos um procedimento que permite comparações fáceis do 
# impacto relativo de cada variável. Isto significa usar o módulo do 
# software CLARIFY para o pacote estatístico STATA 8.0 
# (KING et al, 2000; TOMZ et al, 2003)7. O CLARIFY toma os coeficientes de
# regressão em modelos logísticos e calcula a probabilidade predita de 
# obter qualquer dos resultados na variável dependente dado um 
# conjunto de parâmetros para as variáveis independentes. Assim, o 
# modelo de regressão inicial, por exemplo, poderia predizer que a 
# probabilidade de um brasileiro ser petista é .20, significando que o 
# modelo prediz que um em cada cinco brasileiros é petista8.
# 
# O CLARIFY não pode ser rodado através do SPSS (e nem no R!!!). 
# É claro que se pode computar probabilidades preditas à mão, mas 
# o procedimento pode se tornar muito árduo. 
# Ver King et al (2000) sobre o uso do CLARIFY .
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

source('/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/src/samuels2004.R')


p <- probabilidade_predita(get_petistas() ~ get_opiniao_sobre_lula() + 
                                       get_ideologia_esquerda_direita() +
                                       get_participaca_nao_eleitoral() + 
                                       get_escolaridade() +
                                       get_conhecimento_politico() +
                                       get_avaliacao_governo_fhc() +
                                       get_eficacia_participaca() +
                                       get_participaca_eleitoral())

pandoc.table(p, 
             style = 'grid', 
             # round = c(1:4), 
             digits = 3,
             split.table = Inf, 
             caption = "TABELA 4. Samuels (2004)", 
             plain.ascii = TRUE, 
             justify = 'center')

