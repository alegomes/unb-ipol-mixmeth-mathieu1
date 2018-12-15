############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 2 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(foreign)

get_idp_pop <- function() {
  # “Medimos o apoio ao PT como identificação partidária com o partido. 
  #  Neste caso, o respondente do ESEB primeiro declara que se identifica 
  # com um partido e em seguida declara qual o partido.” (p. 226) 
  idp_pop <- as.data.frame(prop.table(summary(eseb2002$p32)))
  
  # Transformando o rownames numa coluna de dados pra facilitar a selecao
  idp_pop <- data.frame(partido=row.names(idp_pop), percentual=idp_pop[,1])
  
  idp_pop_pt <- sum(idp_pop[which(idp_pop$partido %in% c('PT','Lula')),2])
  idp_pop_pmdb <- sum(idp_pop[which(idp_pop$partido %in% c('PMDB')),2])
  idp_pop_psdb <- sum(idp_pop[which(idp_pop$partido %in% c('PSDB')),2])
  idp_pop_pfl <- sum(idp_pop[which(idp_pop$partido %in% c('PFL')),2])
  idp_pop_pdt <- sum(idp_pop[which(idp_pop$partido %in% c('PDT')),2])
  idp_pop_ptb <- sum(idp_pop[which(idp_pop$partido %in% c('PTB')),2])
  
  outros <- idp_pop[which(!(idp_pop$partido %in% c('PT','Lula','PMDB','PSDB','PFL','PDT','PTB', "NA'S"))),]
  idp_pop_outros <- sum(outros[order(-outros$percentual),][2:12,2])
  
  idp_pop <- c(idp_pop_pt * 100,
               idp_pop_pmdb * 100,
               idp_pop_psdb * 100,
               idp_pop_pfl * 100,
               idp_pop_pdt * 100,
               idp_pop_ptb * 100,
               idp_pop_outros * 100)
  
  return(idp_pop)
}

get_idp_val <- function() {
  # “Medimos o apoio ao PT como identificação partidária com o partido. 
  #  Neste caso, o respondente do ESEB primeiro declara que se identifica 
  # com um partido e em seguida declara qual o partido.” (p. 226) 
  idp_val <- as.data.frame(prop.table(table(eseb2002$p32, useNA='no')))
  
  # Transformando o rownames numa coluna de dados pra facilitar a selecao
  #idp_val <- data.frame(partido=row.names(idp_val), percentual=idp_val[,1])
  
  idp_val_pt <- sum(idp_val[which(idp_val$Var1 %in% c('PT','Lula')),2])
  idp_val_pmdb <- sum(idp_val[which(idp_val$Var1 %in% c('PMDB')),2])
  idp_val_psdb <- sum(idp_val[which(idp_val$Var1 %in% c('PSDB')),2])
  idp_val_pfl <- sum(idp_val[which(idp_val$Var1 %in% c('PFL')),2])
  idp_val_pdt <- sum(idp_val[which(idp_val$Var1 %in% c('PDT')),2])
  idp_val_ptb <- sum(idp_val[which(idp_val$Var1 %in% c('PTB')),2])
  
  outros <- idp_val[which(!(idp_val$Var1 %in% c('PT','Lula','PMDB','PSDB','PFL','PDT','PTB', "NA'S"))),]
  idp_val_outros <- sum(outros[order(-outros$Freq),][2:12,2])
  
  #idp_pop <- c(23.1, 3.8, 3.7, 2.2, 0.5, 0.5, 1.2)
  idp_val <- c(idp_val_pt * 100,
               idp_val_pmdb * 100,
               idp_val_psdb * 100,
               idp_val_pfl * 100,
               idp_val_pdt * 100,
               idp_val_ptb * 100,
               idp_val_outros * 100)
  
  return(idp_val)
}

eseb2002_path <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/eseb2002/01838.sav"
eseb2002 <- read.spss(eseb2002_path, to.data.frame=TRUE)

#  #idp_pop <- c(23.1, 3.8, 3.7, 2.2, 0.5, 0.5, 1.2)
idp_pop <- get_idp_pop()

#idp_val <- c(65.6, 10.9, 10.5, 6.3, 1.5, 1.3, 3.8)
idp_val <- get_idp_val()

perc_votos <- c(18.4, 13.4, 14.3, 13.4, 5.1, 4.6, 30.8)
partidos <- c('PT','PMDB', 'PSDB', 'PFL', 'PDT', 'PTB', 'Outros (11 partidos)')

identificacao_partidaria <- data.frame('Partido'=partidos, 
                                       '%IDPempopulação'=idp_pop, 
                                       '%deIDPválida'=idp_val, 
                                       '%voto, 2002'=perc_votos)

colnames(identificacao_partidaria) <- partidos

pandoc.table(identificacao_partidaria, 
             style = 'grid', 
             round = c(1:4), 
             digits = 3,
             split.table = Inf, 
             caption = "TABELA 2. Samuels (2004)", 
             plain.ascii = TRUE, 
             justify = 'center')
