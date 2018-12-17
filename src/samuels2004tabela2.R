############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 2 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(foreign)

get_idp <- function(useNA) {
  # “Medimos o apoio ao PT como identificação partidária com o partido. 
  #  Neste caso, o respondente do ESEB primeiro declara que se identifica 
  # com um partido e em seguida declara qual o partido.” (p. 226) 
  idp <- as.data.frame(prop.table(table(eseb2002$p32, useNA=useNA)))
  
  # Transformando o rownames numa coluna de dados pra facilitar a selecao
  # idp_pop <- data.frame(partido=row.names(idp_pop), percentual=idp_pop[,1])
  
  idp_pt <- sum(idp[which(idp$Var1 %in% c('PT','Lula')),2])
  idp_pmdb <- sum(idp[which(idp$Var1 %in% c('PMDB')),2])
  idp_psdb <- sum(idp[which(idp$Var1 %in% c('PSDB')),2])
  idp_pfl <- sum(idp[which(idp$Var1 %in% c('PFL')),2])
  idp_pdt <- sum(idp[which(idp$Var1 %in% c('PDT')),2])
  idp_ptb <- sum(idp[which(idp$Var1 %in% c('PTB')),2])
  
  outros <- idp[which(!(idp$Var1 %in% c('PT','Lula','PMDB','PSDB','PFL','PDT','PTB', "NA'S"))),]
  idp_outros <- sum(outros[order(-outros$Freq),][2:12,2])
  
  idp <- c(idp_pt * 100,
           idp_pmdb * 100,
           idp_psdb * 100,
           idp_pfl * 100,
           idp_pdt * 100,
           idp_ptb * 100,
           idp_outros * 100)
  
  return(idp)
}

get_votos <- function() {
  el_2002_basepath <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/votacao_candidato_munzona_2002"
  el_2002_pr_filename <- paste(el_2002_basepath, "votacao_candidato_munzona_2002_BR.txt", sep='/')
  el_2002_pr = read.csv(el_2002_pr_filename, header=FALSE, sep=';')
  
  # Dados extraidos da pagina 'Consulta de Resultados Eleitorais' do TSE disponivel em 
  # http://www.tse.jus.br/eleicoes/eleicoes-anteriores/eleicoes-2002/candidaturas-votacao-e-resultados/resultado-da-eleicao-2002
  # . Comentei as linhas iniciais do arquivo com #
  # . Como as linhas terminavam com uma virgula, tive que adicionar nova coluna no header
  #   i.e. "Cargo,UF,Partido,Nr.,Candidato,Votos Nominais,Situação,%/Válidos,Coligação,X"
  
  tse_2002_basepath <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/"
  tse_2002_filename <- paste(tse_2002_basepath, "Resultado_da_Eleição_(Por_Uf)_2002_tudo.csv" , sep='/')
  tse_2002_turno1 <- read.csv(tse_2002_filename, header=TRUE, sep=',', comment.char = "#", row.names=NULL)
  
  # Alem de terem sido lidos como character, os valores ainda estavam usando virgula como separador de milhar
  tse_2002_turno1$Votos.Nominais <- as.numeric(gsub(',','', tse_2002_turno1$Votos.Nominais))
  
  # Totaliza os votos por partido e por cargo eletivo
  # tse_2002_turno1_total_votos <- aggregate(tse_2002_turno1$Votos.Nominais, by=list(partido=tse_2002_turno1$Partido, cargo=tse_2002_turno1$Cargo), FUN=sum)
  
  # Votacoes no 1º turno das eleicoes do legislativo federal de 2002.
  # Por que legislativas? Porque eh o que esta dito na pag 223 do artigo:
  # "A Tabela 1 mostra a proporção de votos do PT nas eleições legislativas (…)"
  # tse_2002_turno1_leg_pt <- sum(tse_2002_turno1_total_votos[tse_2002_turno1_total_votos$partido == 'PT' & tse_2002_turno1_total_votos$cargo %in% c('Deputado Federal', 'Senador'),3])
  
  tse_2002_turno1_leg_pt <- sum(tse_2002_turno1[which(tse_2002_turno1$Cargo %in% c('Deputado Federal', 'Senador') & tse_2002_turno1$Partido == 'PT'),6])
  tse_2002_turno1_leg_pmdb <- sum(tse_2002_turno1[which(tse_2002_turno1$Cargo %in% c('Deputado Federal', 'Senador') & tse_2002_turno1$Partido == 'PMDB'),6])
  tse_2002_turno1_leg_psdb <- sum(tse_2002_turno1[which(tse_2002_turno1$Cargo %in% c('Deputado Federal', 'Senador') & tse_2002_turno1$Partido == 'PSDB'),6])
  tse_2002_turno1_leg_pfl <- sum(tse_2002_turno1[which(tse_2002_turno1$Cargo %in% c('Deputado Federal', 'Senador') & tse_2002_turno1$Partido == 'PFL'),6])
  tse_2002_turno1_leg_pdt <- sum(tse_2002_turno1[which(tse_2002_turno1$Cargo %in% c('Deputado Federal', 'Senador') & tse_2002_turno1$Partido == 'PDT'),6])
  tse_2002_turno1_leg_ptb <- sum(tse_2002_turno1[which(tse_2002_turno1$Cargo %in% c('Deputado Federal', 'Senador') & tse_2002_turno1$Partido == 'PTB'),6])
  tse_2002_turno1_leg_outros <- sum(tse_2002_turno1[which(tse_2002_turno1$Cargo %in% c('Deputado Federal', 'Senador') & !(tse_2002_turno1$Partido %in% c('PT','Lula','PMDB','PSDB','PFL','PDT','PTB', "NA'S"))),6])
  
  tse_2002_turno1_leg_votos_abs <- c(tse_2002_turno1_leg_pt, 
                                     tse_2002_turno1_leg_pmdb,
                                     tse_2002_turno1_leg_psdb,
                                     tse_2002_turno1_leg_pfl,
                                     tse_2002_turno1_leg_pdt,
                                     tse_2002_turno1_leg_ptb,
                                     tse_2002_turno1_leg_outros)
  
  tse_2002_turno1_leg_votos_perc <- prop.table(tse_2002_turno1_leg_votos_abs)*100
  
  return(tse_2002_turno1_leg_votos_perc)
}

eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)

#  #idp_pop <- c(23.1, 3.8, 3.7, 2.2, 0.5, 0.5, 1.2)
idp_pop <- get_idp('no')

#idp_val <- c(65.6, 10.9, 10.5, 6.3, 1.5, 1.3, 3.8)
idp_val <- get_idp('always')

# perc_votos <- c(18.4, 13.4, 14.3, 13.4, 5.1, 4.6, 30.8)
perc_votos <- get_votos()

partidos <- c('PT','PMDB', 'PSDB', 'PFL', 'PDT', 'PTB', 'Outros (11 partidos)')

identificacao_partidaria <- data.frame('Partido'=partidos, 
                                       'IDP em População'=idp_pop, 
                                       'IDP Válida'=idp_val, 
                                       'Votos, 2002'=perc_votos)

colnames(identificacao_partidaria) <- partidos

pandoc.table(identificacao_partidaria, 
             style = 'grid', 
             round = c(1:4), 
             digits = 3,
             split.table = Inf, 
             caption = "TABELA 2. Samuels (2004)", 
             plain.ascii = TRUE, 
             justify = 'center')
