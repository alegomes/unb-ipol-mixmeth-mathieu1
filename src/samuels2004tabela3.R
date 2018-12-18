############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 3 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

identificacao_partidaria <- sample(rep(as.factor(c('PT','PSDB', 'PMDB', 'PFL', 'Outro', 'Nenhuma')),1000),1000)

categoria_emprego <- factor(c('trabalhador rural', 'pequeno burgues', 'dona de casa', 'setor informal', 
                            'profissional liberal', 'militar/policial', 'mao de obra especializada',
                            'mao de obra nao especializada', 'aposentado', 'estudante',
                            'desempregado'))

categoria_emprego_ <- sample(rep(categoria_emprego,1000),1000)
avaliacao_governo_fhc <- sample(rep(c(1,2,3,4,5,6),1000),1000)
opiniao_sobre_lula <- sample(rep(c(0:12),1000),1000)
ideologia_esquerda_direita <- sample(rep(c(0:10,66),1000),1000)
religicao_catolico  <- sample(rep(factor(c(0:10,66), ordered=T),1000),1000)
religicao_evangelico  <- sample(rep(factor(c(0:10,66), ordered=T),1000),1000)
nao_branco  <- sample(rep(factor(c(0:1)),1000),1000)
mulheres  <- sample(rep(factor(c(0:1)),1000),1000)
idade <- sample(rep(16:94, 1000), 1000)
renda_familiar <- sample(0:25000, 1000)
escolaridade <- sample(rep(1:21,1000), 1000)
conhecimento_politico <- sample(rep(0:10,1000), 1000)
intervencao_governo <- sample(rep(0:10,1000), 1000)
regulamentacao_governo <- sample(rep(0:10,1000), 1000)
nacionalismo_economico <- sample(rep(0:10,1000), 1000)
apoio_clientelismo <- sample(rep(0:10,1000), 1000)
apoio_roubamasfaz <- sample(rep(0:10,1000), 1000)
liberdade_expressao <- sample(rep(0:10,1000), 1000)
valores_hierarquicos <- sample(rep(0:10,1000), 1000)
participaca_eleitoral <- sample(rep(0:10,1000), 1000)
eficacia_participaca <- sample(rep(0:10,1000), 1000)


logit <- glm(identificacao_partidaria ~  categoria_emprego_+
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
