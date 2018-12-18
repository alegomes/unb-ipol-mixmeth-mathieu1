############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 3 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)

# A Tabela 3 apresenta os resultados para o petismo
# 32) Qual o partido que melhor representa a maneira como o(a) Sr(a) pensa? 

eseb2002$petismo[eseb2002$p32 == 'PT'] = 1
eseb2002$petismo[eseb2002$p32 != 'PT'] = 0
#eseb2002$petismo[is.na(eseb2002$p32)] = 0  # O que fazer com os NA's?

petistas <- eseb2002$petismo
  

categoria_emprego <- factor(c('trabalhador rural', 'pequeno burgues', 'dona de casa', 'setor informal', 
                            'profissional liberal', 'militar/policial', 'mao de obra especializada',
                            'mao de obra nao especializada', 'aposentado', 'estudante',
                            'desempregado'))

categoria_emprego_ <- sample(rep(categoria_emprego,2514),2514)

avaliacao_governo_fhc <- sample(rep(c(1,2,3,4,5,6),2514),2514)
opiniao_sobre_lula <- sample(rep(c(0:12),2514),2514)
ideologia_esquerda_direita <- sample(rep(c(0:10,66),2514),2514)
religicao_catolico  <- sample(rep(factor(c(0:10,66), ordered=T),2514),2514)
religicao_evangelico  <- sample(rep(factor(c(0:10,66), ordered=T),2514),2514)
nao_branco  <- sample(rep(factor(c(0:1)),2514),2514)
mulheres  <- sample(rep(factor(c(0:1)),2514),2514)
idade <- sample(rep(16:94, 2514), 2514)
renda_familiar <- sample(0:25000, 2514)
escolaridade <- sample(rep(1:21,2514), 2514)
conhecimento_politico <- sample(rep(0:10,2514), 2514)
intervencao_governo <- sample(rep(0:10,2514), 2514)
regulamentacao_governo <- sample(rep(0:10,2514), 2514)
nacionalismo_economico <- sample(rep(0:10,2514), 2514)
apoio_clientelismo <- sample(rep(0:10,2514), 2514)
apoio_roubamasfaz <- sample(rep(0:10,2514), 2514)
liberdade_expressao <- sample(rep(0:10,2514), 2514)
valores_hierarquicos <- sample(rep(0:10,2514), 2514)
participaca_eleitoral <- sample(rep(0:10,2514), 2514)
eficacia_participaca <- sample(rep(0:10,2514), 2514)


logit <- glm(petistas ~  categoria_emprego_+
                                         avaliacao_governo_fhc +
                                         opiniao_sobre_lula +
                                         ideologia_esquerda_direita +
                                         nao_branco + 
                                         mulheres + 
                                         idade + 
                                         renda_familiar +
                                         escolaridade +
                                         conhecimento_politico +
                                         intervencao_governo +
                                         regulamentacao_governo +
                                         nacionalismo_economico +
                                         apoio_clientelismo +
                                         apoio_roubamasfaz +
                                         liberdade_expressao +
                                         valores_hierarquicos +
                                         participaca_eleitoral +
                                         eficacia_participaca
                                         
              ,family="binomial")

summary(logit)
