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

categoria_emprego_ <- eseb2002$emprego

avaliacao_governo_fhc <- as.numeric(eseb2002$p18)
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
