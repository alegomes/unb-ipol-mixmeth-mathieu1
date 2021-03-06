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

eseb2002$petistas[eseb2002$p32 == 'PT'] = 1
eseb2002$petistas[eseb2002$p32 != 'PT'] = 0
#eseb2002$petismo[is.na(eseb2002$p32)] = 0  # O que fazer com os NA's?

# nossa variavel dependente
petistas <- eseb2002$petistas
  

# 162) Atualmente, qual é a situação profissional do(a) Sr(a): (ESTIMULADA E ÚNICA)
# 164) Qual a sua ocupação, o que o(a) Sr(a) faz no trabalho? (ESPONTÂNEA E ÚNICA)

categorias_emprego <- factor(c('trabalhador rural', 'pequeno burgues', 'dona de casa', 'setor informal', 
                        'profissional liberal', 'militar/policial', 'mao de obra especializada',
                        'mao de obra nao especializada', 'aposentado', 'estudante',
                        'desempregado'))

categorias_emprego <- c('trabalhador rural', 'pequeno burgues', 'dona de casa', 'setor informal', 
                        'profissional liberal', 'militar/policial', 'mao de obra especializada',
                        'mao de obra nao especializada', 'aposentado', 'estudante',
                        'desempregado')

# > levels(eseb2002$p162)
# [1] "Empregado assalariado"                               "Aut\364nomo"                                         "Profissional liberal"                               
# [4] "Empregador/empres\341rio"                            "Estudante"                                           "Desempregado"                                       
# [7] "Dona de casa"                                        "Ajuda algu\351m na fam\355lia com remunera\347\343o" "Ajuda algu\351m na fam\355lia sem remunera\347\343o"
# [10] "Estagi\341rio com remunera\347\343o"                 "Estagi\341rio sem remunera\347\343o"                 "Outro"                                              
# [13] "Aposentado por tempo de trabalho"                    "Aposentado por invalidez"                           

# trabalhador rural

p164 <- levels(eseb2002$p164)

# Olhando pra p164, ha mil possibilidades. E agora? Quais os criterios do autor?
#
# [4] ADESTRADOR DE CAVALO. TRABALHO O ANIMAL PARA EZERCER DISCIPLINA NO ESPORTO
# [39] AGRICULTOR
# [40] AGRICULTOR, CULTIVA, FEIJ\303O E MILHO E CRIA CAPRINOS E OVINOS
# [41] AGRICULTOR, PLANTO, CAPINO, FA\307O A COLHEITA, SECO O FUMO (PEQUENO PROPRIET\301RIO)
# [42] AGRICULTURA (HORTICULTOR)
# [43] AGRICULTURA - PRESTA SERVI\307O
# [44] AGRICULTURA, CULTIVA, FEIJ\303O, MANDIOCA, FUMO
# [85] ARRENDA TERRAS P/ PLANTIO E TAMB\311M TRABALHA COMO TRABALHADOR VOLANTE
# [237] CAPINA, RO\307A E PASTO
# [238] CAPINAR, ENXADA, FOICE, SEMANA NUM LUGAR, OUTRA EM OUTRA...
# [239] CARNEBRA IMOVIN - IMOBILIZAR
# [240] CARPINAR MATO, PLANTA\307\303O DE MILHO E FEIJ\303O
# [249] CASEIRA CUIDA DA CH\301CARA
# [250] CASEIRA DA SEDE DA FAZENDA (DOM\311STICA DA SEDE)
# [251] CASEIRA DE UMA CH\301CARA , NO QUAL ELA MORA NOS FUNDOS
# [252] CASEIRO
# [253] CASEIRO DE CASA
# [254] CASEIRO DE S\315TIO
# [300] COMPRA E VENDA DE GADO
# [339] CORTA A MADEIRA
# [340] CORTADOR DE CANA
# [365] CUIDA DAS CABRAS TIRA TOCO CORTA VARA, LIMPO A RO\307A, PLANTA
# [366] CUIDA DE GADO
# [367] CUIDA DE GADO ( ADMINISTRA )
# [511] FAZ DE TUDO, TRABALHADOR RURAL
# [597] LAVOURA
# [598] LAVOURA / CUIDO DE BAR
# [599] LAVOURA, CAPINA, CRIA ANIMAIS PARA OS OUTROS
# [600] LAVRADOR
# [601] LAVRADOR (PLANTA, COLHE, VENDE, MILHO, ARROZ) LEITEIRO
# [602] LAVRADOR (SERVI\307OS GERAIS DE RO\307A)
# [603] LAVRADOR DE AGRICULTURA
# [604] LAVRADOR DE CAF\311
# [605] LAVRADOR DE RO\307A - CUIDO DO LARANJAL CUIDO DO GADO - PESCO
# [606] LAVRADOR, MEXE COM PLANTA\307\303O
# [607] LAVRADOR, TUDO QUANTO \311 TIPO DE TRABALHO NA AGRICULTURA EU FA\307O
# [608] LAVRADOR- PLANTA COLHE E VENDE PARA SEU SUSTENTO
# [609] LAVRADORA, CAPINA, OUTROS DIAS FICA EM CASA, OUTROS DIAS CATA ANDU, FEIJ\303O, QUANDO TEM
# [614] LIMPA A RO\307A, PLANTA, COLHE.(MILHO E FEIJ\303O), MAS TUDO DEPENDE DA CHUVA DE DEUS
# [619] LIMPEZA, FAXINA, CAPINA O QUINTAL
# [744] PALNTAR, REGAR, COLHER OR FRUTOS
# [749] PECU\301RIA (PEQUENO PROPRIET\301RIO RURAL)
# [773] PESCADOR
# [774] PESCADOR DE CAMAR\303O
# [787] PLANTA E VENDE (VERDURA E LEGUMES)
# [788] PLANTA, CULTIVA, PRODUTOR AGRICOLA
# [789] PLANTAR, COLHER, VENDER MORANGOS EM PROPRIEDADE DE TERCEIROS (ACENTAMENTO)
# [790] PLANTAR, COLHER, VENDER MORANGOS EM SUA PROPRIEDADE E CUIDAR DOS SERVI\307OS DA CASA
# [791] PLANTAR, COLHER, VENDER MORANGOS, ARAR A TERRA PARA O PLANTIO DE MILHO E FEIJ\303O
# [792] PLANTAR, COLHER, VENDER MORANGOS, TRABALHA EM UM TERRENO ARRENDADO
# [993] SUPERVIS\303O, DO FUNCION\301RIO DA PROP. RURAL PRODU\307\303O PECU\301RIA (CORTE E LEITE)
# (...)
eseb2002$emprego[eseb2002$p164 == p164[4] | 
                 eseb2002$p164 == p164[39] |
                 eseb2002$p164 == p164[40] |
                 eseb2002$p164 == p164[41] |
                 eseb2002$p164 == p164[42] |
                 eseb2002$p164 == p164[43] |
                 eseb2002$p164 == p164[44] |
                 eseb2002$p164 == p164[85] 
                 # (...)  Vai saber o que foi considerado....
                 ] <- categorias_emprego[1]

# pequeno burgues
eseb2002$emprego[eseb2002$p162 == levels(eseb2002$p162)[4]] <- categorias_emprego[2]  # >:-(

# dona de casa
eseb2002$emprego[eseb2002$p162 == levels(eseb2002$p162)[7]] <- categorias_emprego[3]

# setor informal
eseb2002$emprego[eseb2002$p164 == p164[78] | # AMBULANTE VENDE PL\301STICOS
                 eseb2002$p164 == p164[255]  # CATADOR DE PAPEL\303O 
                 # (...)  Sabe-se la o que foi considerado....
                 ] <- categorias_emprego[4]

# profissional liberal
eseb2002$emprego[eseb2002$p162 == levels(eseb2002$p162)[3]] <- categorias_emprego[5]

# militar ou policial
# [32] AGENTE DE POLICIA
# [37] AGENTE POLICIAL
# [648] MILITAR
# [649] MILITAR DO EX\311RCITO
# [650] MILITAR, SOLDADO
# [793] PM-POLICIAL SOLDADO
# [794] POLICIA MILITAR
# [795] POLICIAL CIVIL, TRABALHA INTERNAMENTE NA ACADEMIA DE POL\315CIA (RESPONS\301VEL POR CUSOS PROFISSIONALIZANTES DA POL\315CIA)
# [796] POLICIAL DA AERON\301UTICA (SEGURAN\307A DENTRO DO QUARTEL)
# [797] POLICIAL MILITAR
# [798] POLICIAL MILITAR, GUARDA DE TR\302NSITO
# [799] POLICIAMENTO DE RUA
eseb2002$emprego[eseb2002$p164 == p164[32] | 
                 eseb2002$p164 == p164[37] |
                 eseb2002$p164 == p164[648] | 
                 eseb2002$p164 == p164[649] |
                 eseb2002$p164 == p164[650] |
                 eseb2002$p164 == p164[793] |
                 eseb2002$p164 == p164[794] |
                 eseb2002$p164 == p164[795] |
                 eseb2002$p164 == p164[796] |
                 eseb2002$p164 == p164[797] |
                 eseb2002$p164 == p164[798] |
                 eseb2002$p164 == p164[799]
                 # (...)  Sabe-se la o que foi considerado....
                 ] <- categorias_emprego[6]

# mao de obra especializada
# Dificil descobrir quais os criterios utilizados. A titulo de exemplo,
# pra mim, nao ha nada mais especializado do que um tratador de camaroes
# ou um bicheiro :-)
# [186] A\307OADOR - RESPONS\301VEL POR COLOCAR COMIDA PARA CAMAR\325ES
# [234] CAMBISTA - JOGO DO BICHO
eseb2002$emprego[eseb2002$p164 == p164[186] |
                 eseb2002$p164 == p164[234] 
                 # (...)  Sabe-se la o que foi considerado....
                 ] <- categorias_emprego[7]

# mao de obra nao especializada
# [2] ACOMPANHANTE DE IDOSO
# [252] CASEIRO
eseb2002$emprego[eseb2002$p164 == p164[2] |
                 eseb2002$p164 == p164[252] 
                 # (...)  Sabe-se la o que foi considerado....
                ] <- categorias_emprego[8]
# aposentado
eseb2002$emprego[eseb2002$p162 == levels(eseb2002$p162)[13] | eseb2002$p162 == levels(eseb2002$p162)[14]] <- categorias_emprego[9]

# estudante
eseb2002$emprego[eseb2002$p162 == levels(eseb2002$p162)[5]] <- categorias_emprego[10]

# desempregado
eseb2002$emprego[eseb2002$p162 == levels(eseb2002$p162)[6]] <- categorias_emprego[11]

categoria_emprego_ <- factor(eseb2002$emprego, levels=categorias_emprego)

avaliacao_governo_fhc <- as.numeric(eseb2002$p18)
opiniao_sobre_lula <- as.numeric(eseb2002$p43a)
ideologia_esquerda_direita <- as.numeric(eseb2002$p50v1) # Pq nao p50v2 ou uma combinacao de ambas?

eseb2002$catolicos[eseb2002$p182 == levels(eseb2002$p182)[8]] <- 1
eseb2002$catolicos[eseb2002$p182 != levels(eseb2002$p182)[8]] <- 0
religicao_catolico  <- eseb2002$catolicos

eseb2002$evangelicos[eseb2002$p182 == levels(eseb2002$p182)[2] | eseb2002$p182 == levels(eseb2002$p182)[3]] <- 1
eseb2002$evangelicos[eseb2002$p182 != levels(eseb2002$p182)[2] & eseb2002$p182 != levels(eseb2002$p182)[3]] <- 0
religicao_evangelico  <- eseb2002$evangelicos

eseb2002$nao_branco[eseb2002$p189 != levels(eseb2002$p189)[3]] <- 1
eseb2002$nao_branco[eseb2002$p189 == levels(eseb2002$p189)[3]] <- 0
nao_branco <- eseb2002$nao_branco

eseb2002$mulheres[eseb2002$p158 == levels(eseb2002$p158)[2]] <- 1
eseb2002$mulheres[eseb2002$p158 != levels(eseb2002$p158)[2]] <- 0
mulheres <- eseb2002$mulheres

idade <- eseb2002$p157

renda_familiar <- as.numeric(eseb2002$p176)

escolaridade <- as.numeric(eseb2002$p159)

# Nao identificamos a fonte do conhecimento politico 
# Poderia ser a questao 41, mas haveria a necessidade de se
# ter um gabarito para afericao das respostas com uma escala
# comum de avaliacao a ser utilizada na regressao
conhecimento_politico <- sample(rep(0:10,2514), 2514) 

# Nao identificamos a fonte do conhecimento politico 
# Poderia ser a questao 107, mas tambem haveria a necessidade
# de gabarito
intervencao_governo <-  as.numeric(eseb2002$p107a) +
                        as.numeric(eseb2002$p107b) +
                        as.numeric(eseb2002$p107c) +
                        as.numeric(eseb2002$p107d) +
                        as.numeric(eseb2002$p107e) +
                        as.numeric(eseb2002$p107f) +
                        as.numeric(eseb2002$p107g) +
                        as.numeric(eseb2002$p107h) +
                        as.numeric(eseb2002$p107i) +
                        as.numeric(eseb2002$p107j) +
                        as.numeric(eseb2002$p107k) +
                        as.numeric(eseb2002$p107l) +
                        as.numeric(eseb2002$p107m) +
                        as.numeric(eseb2002$p107n)

# Os os itens pro regulacao foram somados e, os contrarios,
# subtraidos para a formacao do indicador.
regulamentacao_governo <- as.numeric(eseb2002$p108a) +
                          as.numeric(eseb2002$p108b) -
                          as.numeric(eseb2002$p108c) +
                          as.numeric(eseb2002$p108d) +
                          as.numeric(eseb2002$p108e) -
                          as.numeric(eseb2002$p108f) +
                          as.numeric(eseb2002$p108g) 

nacionalismo_economico <- as.numeric(eseb2002$p109a) +
                          as.numeric(eseb2002$p109b) -
                          as.numeric(eseb2002$p109c) -
                          as.numeric(eseb2002$p109d) +
                          as.numeric(eseb2002$p109e) +
                          as.numeric(eseb2002$p109f) -
                          as.numeric(eseb2002$p109g) 

# Talvez tambem a 127
apoio_clientelismo <- as.numeric(eseb2002$p93) +
                      as.numeric(eseb2002$p95) +
                      as.numeric(eseb2002$p97) +
                      as.numeric(eseb2002$p99) +
                      as.numeric(eseb2002$p103) +
                      as.numeric(eseb2002$p104) 
  
apoio_roubamasfaz <- as.numeric(eseb2002$p105b)

# Da pra considerar varias outras perguntas tmb
liberdade_expressao <-  as.numeric(eseb2002$p106d)

# Tenho duvidas...
valores_hierarquicos <- as.numeric(eseb2002$p134a) +
                        as.numeric(eseb2002$p134b) +
                        as.numeric(eseb2002$p134c) +
                        as.numeric(eseb2002$p134d) +
                        as.numeric(eseb2002$p134e) +
                        as.numeric(eseb2002$p134f) +
                        as.numeric(eseb2002$p134g)

participaca_eleitoral <- as.numeric(eseb2002$p06)

participacao_nao_eleitoral <- as.numeric(eseb2002$p75a) +
                              as.numeric(eseb2002$p75b) +
                              as.numeric(eseb2002$p75c) +
                              as.numeric(eseb2002$p77a) +
                              as.numeric(eseb2002$p77b) +
                              as.numeric(eseb2002$p77c) +
                              as.numeric(eseb2002$p77d) +
                              as.numeric(eseb2002$p77e) +
                              as.numeric(eseb2002$p78a) +
                              as.numeric(eseb2002$p78b) +
                              as.numeric(eseb2002$p78c) 

eficacia_participaca <- as.numeric(eseb2002$p21)


logit <- glm(petistas ~  categoria_emprego_+
                                         avaliacao_governo_fhc +
                                         opiniao_sobre_lula +
                                         ideologia_esquerda_direita +
                                         religicao_catolico +
                                         religicao_evangelico +
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
                                         participacao_nao_eleitoral + 
                                         eficacia_participaca
                                         
              ,family="binomial")

summary(logit)

# Call:
#   glm(formula = petistas ~ categoria_emprego_ + avaliacao_governo_fhc + 
#         opiniao_sobre_lula + ideologia_esquerda_direita + religicao_catolico + 
#         religicao_evangelico + nao_branco + mulheres + idade + renda_familiar + 
#         escolaridade + conhecimento_politico + intervencao_governo + 
#         regulamentacao_governo + nacionalismo_economico + apoio_clientelismo + 
#         apoio_roubamasfaz + liberdade_expressao + valores_hierarquicos + 
#         participaca_eleitoral + participacao_nao_eleitoral + eficacia_participaca, 
#       family = "binomial")
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1455  -0.6759   0.2518   0.6827   2.7029  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                 -3.512e+00  3.040e+00  -1.156  0.24787    
# categoria_emprego_dona de casa               2.995e-01  1.180e+00   0.254  0.79962    
# categoria_emprego_profissional liberal      -1.138e-02  1.532e+00  -0.007  0.99407    
# categoria_emprego_militar/policial          -1.763e+00  1.962e+00  -0.899  0.36878    
# categoria_emprego_mao de obra especializada  1.666e+01  1.455e+03   0.011  0.99086    
# categoria_emprego_aposentado                 8.301e-01  1.192e+00   0.696  0.48632    
# categoria_emprego_estudante                 -3.557e-02  1.298e+00  -0.027  0.97814    
# categoria_emprego_desempregado               1.231e-01  1.148e+00   0.107  0.91456    
# avaliacao_governo_fhc                       -2.942e-01  1.328e-01  -2.215  0.02675 *  
#   opiniao_sobre_lula                           4.892e-01  9.421e-02   5.193 2.07e-07 ***
#   ideologia_esquerda_direita                  -1.512e-02  5.536e-02  -0.273  0.78470    
# religicao_catolico                          -2.767e-01  6.221e-01  -0.445  0.65649    
# religicao_evangelico                         5.111e-02  7.438e-01   0.069  0.94522    
# nao_branco                                   2.665e-01  4.691e-01   0.568  0.56992    
# mulheres                                    -6.338e-03  5.237e-01  -0.012  0.99034    
# idade                                       -3.017e-02  2.026e-02  -1.490  0.13633    
# renda_familiar                               1.314e-02  4.830e-03   2.721  0.00651 ** 
#   escolaridade                                 6.080e-02  5.977e-02   1.017  0.30908    
# conhecimento_politico                        1.361e-01  6.556e-02   2.077  0.03782 *  
#   intervencao_governo                         -6.034e-02  4.090e-02  -1.475  0.14021    
# regulamentacao_governo                       2.662e-02  5.384e-02   0.494  0.62104    
# nacionalismo_economico                       5.311e-02  4.899e-02   1.084  0.27826    
# apoio_clientelismo                           2.420e-01  1.077e-01   2.248  0.02459 *  
#   apoio_roubamasfaz                           -4.657e-02  1.540e-01  -0.302  0.76233    
# liberdade_expressao                         -1.666e-01  1.299e-01  -1.283  0.19953    
# valores_hierarquicos                        -7.218e-02  3.604e-02  -2.003  0.04519 *  
#   participaca_eleitoral                        5.108e-02  2.212e-01   0.231  0.81738    
# participacao_nao_eleitoral                  -3.966e-02  9.093e-02  -0.436  0.66272    
# eficacia_participaca                        -7.302e-02  2.137e-01  -0.342  0.73264    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 252.77  on 182  degrees of freedom
# Residual deviance: 160.97  on 154  degrees of freedom
# (2331 observations deleted due to missingness)
# AIC: 218.97
# 
# Number of Fisher Scoring iterations: 14
