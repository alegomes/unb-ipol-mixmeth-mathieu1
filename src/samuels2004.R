############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Operacoes comuns para reproducao do artigo do Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(foreign)

eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)

get_petistas <- function() {
  # A Tabela 3 apresenta os resultados para o petismo
  # 32) Qual o partido que melhor representa a maneira como o(a) Sr(a) pensa? 
  
  eseb2002$petistas[eseb2002$p32 == 'PT'] = 1
  eseb2002$petistas[eseb2002$p32 != 'PT'] = 0
  #eseb2002$petismo[is.na(eseb2002$p32)] = 0  # O que fazer com os NA's?
  
  # nossa variavel dependente
  return(eseb2002$petistas)
}

get_categorias_emprego <- function() {
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
  
  return(factor(eseb2002$emprego, levels=categorias_emprego))
}

get_avaliacao_governo_fhc <- function() {
  return(as.numeric(eseb2002$p18))
}

get_opiniao_sobre_lula <- function() {
  return(as.numeric(eseb2002$p43a))
}

get_ideologia_esquerda_direita <- function() {
  return(as.numeric(eseb2002$p50v1)) # Pq nao p50v2 ou uma combinacao de ambas?
}

get_religiao_catolico <- function() {
  eseb2002$catolicos[eseb2002$p182 == levels(eseb2002$p182)[8]] <- 1
  eseb2002$catolicos[eseb2002$p182 != levels(eseb2002$p182)[8]] <- 0
  return(eseb2002$catolicos)
}

get_religiao_evangelico <- function() {
  eseb2002$evangelicos[eseb2002$p182 == levels(eseb2002$p182)[2] | eseb2002$p182 == levels(eseb2002$p182)[3]] <- 1
  eseb2002$evangelicos[eseb2002$p182 != levels(eseb2002$p182)[2] & eseb2002$p182 != levels(eseb2002$p182)[3]] <- 0
  return(eseb2002$evangelicos)
}

get_nao_branco <- function() {
  eseb2002$nao_branco[eseb2002$p189 != levels(eseb2002$p189)[3]] <- 1
  eseb2002$nao_branco[eseb2002$p189 == levels(eseb2002$p189)[3]] <- 0
  return(eseb2002$nao_branco)
}

get_mulheres <- function() {
  eseb2002$mulheres[eseb2002$p158 == levels(eseb2002$p158)[2]] <- 1
  eseb2002$mulheres[eseb2002$p158 != levels(eseb2002$p158)[2]] <- 0
  return(eseb2002$mulheres)
}

get_idade <- function() {
  return(eseb2002$p157)
}

get_renda_familiar <- function() {
  return(as.numeric(eseb2002$p176))
}

get_escolaridade <- function() {
  return(as.numeric(eseb2002$p159))
}

# Nao identificamos a fonte do conhecimento politico 
# Poderia ser a questao 41, mas haveria a necessidade de se
# ter um gabarito para afericao das respostas com uma escala
# comum de avaliacao a ser utilizada na regressao
get_conhecimento_politico <- function() {
  return(sample(rep(0:10,2514), 2514))
}

# Nao identificamos a fonte do conhecimento politico 
# Poderia ser a questao 107, mas tambem haveria a necessidade
# de gabarito
get_intervencao_governo <- function() {
  return(as.numeric(eseb2002$p107a) +
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
           as.numeric(eseb2002$p107n))
}

# Os os itens pro regulacao foram somados e, os contrarios,
# subtraidos para a formacao do indicador.
get_regulamentacao_governo <- function() {
  return( as.numeric(eseb2002$p108a) +
            as.numeric(eseb2002$p108b) -
            as.numeric(eseb2002$p108c) +
            as.numeric(eseb2002$p108d) +
            as.numeric(eseb2002$p108e) -
            as.numeric(eseb2002$p108f) +
            as.numeric(eseb2002$p108g))
}

get_nacionalismo_economico <- function() {
  return( as.numeric(eseb2002$p109a) +
            as.numeric(eseb2002$p109b) -
            as.numeric(eseb2002$p109c) -
            as.numeric(eseb2002$p109d) +
            as.numeric(eseb2002$p109e) +
            as.numeric(eseb2002$p109f) -
            as.numeric(eseb2002$p109g))
}

# Talvez tambem a 127
get_apoio_clientelismo <- function() {
  return( as.numeric(eseb2002$p93) +
            as.numeric(eseb2002$p95) +
            as.numeric(eseb2002$p97) +
            as.numeric(eseb2002$p99) +
            as.numeric(eseb2002$p103) +
            as.numeric(eseb2002$p104))
}

get_apoio_roubamasfaz <- function() {
  return(as.numeric(eseb2002$p105b))
}

# Da pra considerar varias outras perguntas tmb
get_liberdade_expressao <-  function() {
  return(as.numeric(eseb2002$p106d))
}

# Tenho duvidas...
get_valores_hierarquicos <- function() {
  return( as.numeric(eseb2002$p134a) +
            as.numeric(eseb2002$p134b) +
            as.numeric(eseb2002$p134c) +
            as.numeric(eseb2002$p134d) +
            as.numeric(eseb2002$p134e) +
            as.numeric(eseb2002$p134f) +
            as.numeric(eseb2002$p134g))
}

get_participacao_eleitoral <- function() {
  return(as.numeric(eseb2002$p06))
}

get_participacao_nao_eleitoral <- function() {
  return( as.numeric(eseb2002$p75a) +
            as.numeric(eseb2002$p75b) +
            as.numeric(eseb2002$p75c) +
            as.numeric(eseb2002$p77a) +
            as.numeric(eseb2002$p77b) +
            as.numeric(eseb2002$p77c) +
            as.numeric(eseb2002$p77d) +
            as.numeric(eseb2002$p77e) +
            as.numeric(eseb2002$p78a) +
            as.numeric(eseb2002$p78b) +
            as.numeric(eseb2002$p78c))
}

get_eficacia_participacao <- function() {
  return(as.numeric(eseb2002$p21))
}

logit <- function() {
  data <- data.frame(petistas=get_petistas(),
                     categorias_emprego_=get_categorias_emprego(),
                     avaliacao_governo_fhc=get_avaliacao_governo_fhc(),
                     opiniao_sobre_lula=get_opiniao_sobre_lula(),
                     ideologia_esquerda_direita=get_ideologia_esquerda_direita(),
                     religiao_catolico=get_religiao_catolico(),
                     religiao_evangelico=get_religiao_evangelico(),
                     nao_branco=get_nao_branco(), 
                     mulheres=get_mulheres(), 
                     idade=get_idade(), 
                     renda_familiar=get_renda_familiar(),
                     escolaridade=get_escolaridade(),
                     conhecimento_politico=get_conhecimento_politico(),
                     intervencao_governo=get_intervencao_governo(),
                     regulamentacao_governo=get_regulamentacao_governo(),
                     nacionalismo_economico=get_nacionalismo_economico(),
                     apoio_clientelismo=get_apoio_clientelismo(),
                     apoio_roubamasfaz=get_apoio_roubamasfaz(),
                     liberdade_expressao=get_liberdade_expressao(),
                     valores_hierarquicos=get_valores_hierarquicos(),
                     participacao_eleitoral=get_participacao_eleitoral(),
                     participacao_nao_eleitoral=get_participacao_nao_eleitoral(), 
                     eficacia_participacao=get_eficacia_participacao())
  
  logit <- glm(petistas ~  categorias_emprego_ +
                 avaliacao_governo_fhc +
                 opiniao_sobre_lula +
                 ideologia_esquerda_direita +
                 religiao_catolico +
                 religiao_evangelico +
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
                 participacao_eleitoral +
                 participacao_nao_eleitoral + 
                 eficacia_participacao
               ,data=data             
               ,family="binomial")
  
  return(logit)
}

get_means <- function() {
  return(data.frame(opiniao_sobre_lula=mean(get_opiniao_sobre_lula(), na.rm=TRUE),
                    ideologia_esquerda_direita=mean(get_ideologia_esquerda_direita(), na.rm=TRUE),
                    participacao_nao_eleitoral=mean(get_participacao_nao_eleitoral(), na.rm=TRUE),
                    escolaridade=mean(get_escolaridade(), na.rm=TRUE),
                    conhecimento_politico=mean(get_conhecimento_politico(), na.rm=TRUE),
                    avaliacao_governo_fhc=mean(get_avaliacao_governo_fhc(), na.rm=TRUE),
                    eficacia_participacao=mean(get_eficacia_participacao(), na.rm=TRUE),
                    participacao_eleitoral=mean(get_participacao_eleitoral(), na.rm=TRUE),
                    categorias_emprego_=factor(c(99)),
                    religiao_catolico=mean(get_religiao_catolico(), na.rm=TRUE),
                    religiao_evangelico=mean(get_religiao_evangelico(), na.rm=TRUE),
                    nao_branco=mean(get_nao_branco(), na.rm=TRUE),
                    mulheres=mean(get_mulheres(), na.rm=TRUE),
                    idade=mean(get_idade(), na.rm=TRUE),
                    renda_familiar=mean(get_renda_familiar(), na.rm=TRUE),
                    intervencao_governo=mean(get_intervencao_governo(), na.rm=TRUE),
                    regulamentacao_governo=mean(get_regulamentacao_governo(), na.rm=TRUE),
                    nacionalismo_economico=mean(get_nacionalismo_economico(), na.rm=TRUE),
                    apoio_clientelismo=mean(get_apoio_clientelismo(), na.rm=TRUE),
                    apoio_roubamasfaz=mean(get_apoio_roubamasfaz(), na.rm=TRUE),
                    liberdade_expressao=mean(get_liberdade_expressao(), na.rm=TRUE),
                    valores_hierarquicos=mean(get_valores_hierarquicos(), na.rm=TRUE)))
}


test_probability <- function(model, newcol=NA) {
  # Array com todos os X's medios
  data <- get_means()
  
  # Substitui um dos valores medios pelo valor passado como parametro,
  # i.e. o valor maximo da variavel
  if(!is.na(newcol)) {
    data[names(newcol)[1]] <- newcol
  }
  
  model$xlevels[["categorias_emprego_"]] <- union(model$xlevels[["categorias_emprego_"]], levels(data$categorias_emprego_))
  return(predict(model, data, type="response"))
}

test_probability_of_means <- function(model) {
  return(test_probability(model))
}

test_probability_of_opiniao_sobre_lula <- function(model) {
  return(test_probability(model,c(opiniao_sobre_lula=max(get_opiniao_sobre_lula(), na.rm=T))))
}


test_probability_of_ideologia_esquerda_direita <- function(model) {
  return(test_probability(model,c(ideologia_esquerda_direita=max(get_ideologia_esquerda_direita(), na.rm=T))))
}


test_probability_of_participacao_nao_eleitoral <- function(model) {
  return(test_probability(model,c(participacao_nao_eleitoral=max(get_participacao_nao_eleitoral(), na.rm=T))))
}


test_probability_of_escolaridade <- function(model) {
  return(test_probability(model,c(escolaridade=max(get_escolaridade(), na.rm=T))))
}


test_probability_of_conhecimento_politico <- function(model) {
  return(test_probability(model,c(conhecimento_politico=max(get_conhecimento_politico(), na.rm=T))))
}


test_probability_of_avaliacao_governo_fhc <- function(model) {
  return(test_probability(model,c(avaliacao_governo_fhc=max(get_avaliacao_governo_fhc(), na.rm=T))))
}


test_probability_of_eficacia_participacao <- function(model) {
  return(test_probability(model,c(eficacia_participacao=max(get_eficacia_participacao(), na.rm=T))))
}


test_probability_of_participacao_eleitoral <- function(model) {
  return(test_probability(model,c(participacao_eleitoral=max(get_participacao_eleitoral(), na.rm=T))))
}



