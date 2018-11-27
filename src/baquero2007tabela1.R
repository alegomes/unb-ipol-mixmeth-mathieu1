############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 1 do artigo Baquero (2007)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(foreign)
library(pander)

####### PRESIDENTE 2002 #######

el_2002_basepath <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/votacao_candidato_munzona_2002"
el_2002_pr_filename <- paste(el_2002_basepath, "votacao_candidato_munzona_2002_BR.txt", sep='/')
el_2002_pr = read.csv(el_2002_pr_filename, header=FALSE, sep=';')

# Total de votos por candidato e por turno
el_2002_pr_votos <- aggregate(el_2002_pr$V29, by=list(el_2002_pr$V15, el_2002_pr$V4), FUN=sum)

# Novos nomes para as 3 colunas do data frame
names(el_2002_pr_votos) <- c('Candidato', 'Turno', 'Votos')

# Foca no 1o turno
el_2002_pr_votos_turno1 <- el_2002_pr_votos[el_2002_pr_votos$Turno == 1, ]

# Calculo do percentual de votos de cada candidato do 1o turno
el_2002_pr_votos_turno1$VotosPercentual = el_2002_pr_votos_turno1$Votos / sum(el_2002_pr_votos_turno1$Votos)

# Acrescenta Ano
el_2002_pr_votos_turno1$Ano = 2002

# Acrescenta coluna Cargo
el_2002_pr_votos_turno1$Cargo = 'PRESIDENTE (LULA)'

# So o LULA nos interessa
el_2002_pr_votos_turno1_lula = el_2002_pr_votos_turno1[el_2002_pr_votos_turno1$Candidato == 'LULA',]


####### PRESIDENTE 2006 #######

el_2006_basepath <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/votacao_candidato_munzona_2006"
el_2006_pr_filename <- paste(el_2006_basepath, "votacao_candidato_munzona_2006_BR.txt", sep='/')
el_2006_pr <- read.csv(el_2006_pr_filename, header=FALSE, sep=';')

# Total de votos por candidato e por turno
el_2006_pr_votos <- aggregate(el_2006_pr$V29, by=list(el_2006_pr$V15, el_2006_pr$V4), FUN=sum)

# Novos nomes para as 3 colunas do data frame
names(el_2006_pr_votos) <- c('Candidato', 'Turno', 'Votos')

# Foca no 1o turno
el_2006_pr_votos_turno1 <- el_2006_pr_votos[el_2006_pr_votos$Turno == 1, ]

# Calculo do percentual de votos de cada candidato do 1o turno
el_2006_pr_votos_turno1$VotosPercentual <- el_2006_pr_votos_turno1$Votos / sum(el_2006_pr_votos_turno1$Votos)

# Acrescenta Ano
el_2006_pr_votos_turno1$Ano <- 2006

# Acrescenta coluna Cargo
el_2006_pr_votos_turno1$Cargo <- 'PRESIDENTE (LULA)'

# So o LULA nos interessa
el_2006_pr_votos_turno1_lula <- el_2006_pr_votos_turno1[el_2006_pr_votos_turno1$Candidato == 'LULA',]



####### DEPUTADOS FEDERAIS 2002 #######

el_2002_basepath <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/votacao_candidato_munzona_2002"

el_2002_uf_files <- c(
  'votacao_candidato_munzona_2002_AC.txt',
  'votacao_candidato_munzona_2002_AL.txt',
  'votacao_candidato_munzona_2002_AM.txt',
  'votacao_candidato_munzona_2002_AP.txt',
  'votacao_candidato_munzona_2002_BA.txt',
  'votacao_candidato_munzona_2002_CE.txt',
  'votacao_candidato_munzona_2002_DF.txt',
  'votacao_candidato_munzona_2002_ES.txt',
  'votacao_candidato_munzona_2002_GO.txt',
  'votacao_candidato_munzona_2002_MA.txt',
  'votacao_candidato_munzona_2002_MG.txt',
  'votacao_candidato_munzona_2002_MS.txt',
  'votacao_candidato_munzona_2002_MT.txt',
  'votacao_candidato_munzona_2002_PA.txt',
  'votacao_candidato_munzona_2002_PB.txt',
  'votacao_candidato_munzona_2002_PE.txt',
  'votacao_candidato_munzona_2002_PI.txt',
  'votacao_candidato_munzona_2002_PR.txt',
  'votacao_candidato_munzona_2002_RJ.txt',
  'votacao_candidato_munzona_2002_RN.txt',
  'votacao_candidato_munzona_2002_RO.txt',
  'votacao_candidato_munzona_2002_RR.txt',
  'votacao_candidato_munzona_2002_RS.txt',
  'votacao_candidato_munzona_2002_SC.txt',
  'votacao_candidato_munzona_2002_SE.txt',
  'votacao_candidato_munzona_2002_SP.txt',
  'votacao_candidato_munzona_2002_TO.txt')

# Criacao de data frame vazio para acumular os registros de cada um dos arquivos acima
el_2002_uf_all <- data.frame(matrix(NA, nrow=0, ncol=29))

for (file in el_2002_uf_files) {
  print(file)
  el_2002_uf_filename <- paste(el_2002_basepath, file, sep='/')
  el_2002_uf <- read.csv(el_2002_uf_filename, header=FALSE, sep=';')

  el_2002_uf_all <- rbind(el_2002_uf_all, el_2002_uf)
}

# Apenas votacoes de DEPUTADO FEDERAL
el_2002_uf_df <- el_2002_uf_all[el_2002_uf_all$V16 == 'DEPUTADO FEDERAL',]

# Total de votos por candidato e por turno
el_2002_uf_df_votos <- aggregate(el_2002_uf_df$V29, by=list(el_2002_uf_df$V24, el_2002_uf_df$V4), FUN=sum)

# Novos nomes para as 3 colunas do data frame
names(el_2002_uf_df_votos) <- c('Partido', 'Turno', 'Votos')

# Foca no 1o turno (so tem votacao pra Dep Federal no 1o turno)
el_2002_uf_df_votos_turno1 <- el_2002_uf_df_votos[el_2002_uf_df_votos$Turno == 1, ]

# Calculo do percentual de votos de cada candidato do 1o turno
el_2002_uf_df_votos_turno1$VotosPercentual <- el_2002_uf_df_votos_turno1$Votos / sum(el_2002_uf_df_votos_turno1$Votos)

# So PT nos interessa
el_2002_uf_df_votos_turno1_pt <- el_2002_uf_df_votos_turno1[el_2002_uf_df_votos_turno1$Partido == 'PT',]

# Acrescenta coluna Ano
el_2002_uf_df_votos_turno1_pt$Ano <- 2002

# Acrescenta coluna Cargo
el_2002_uf_df_votos_turno1_pt$Cargo <- 'DEPUTADO FEDERAL (PT)'

####### DEPUTADOS FEDERAIS 2006 #######

el_2006_basepath <- "/Users/alegomes/GDrive/2018/unb/ipol/disc métodos mistos/provas/1. Mathieu/data/votacao_candidato_munzona_2006"

el_2006_uf_files <- c(
  'votacao_candidato_munzona_2006_AC.txt',
  'votacao_candidato_munzona_2006_AL.txt',
  'votacao_candidato_munzona_2006_AM.txt',
  'votacao_candidato_munzona_2006_AP.txt',
  'votacao_candidato_munzona_2006_BA.txt',
  'votacao_candidato_munzona_2006_CE.txt',
  'votacao_candidato_munzona_2006_DF.txt',
  'votacao_candidato_munzona_2006_ES.txt',
  'votacao_candidato_munzona_2006_GO.txt',
  'votacao_candidato_munzona_2006_MA.txt',
  'votacao_candidato_munzona_2006_MG.txt',
  'votacao_candidato_munzona_2006_MS.txt',
  'votacao_candidato_munzona_2006_MT.txt',
  'votacao_candidato_munzona_2006_PA.txt',
  'votacao_candidato_munzona_2006_PB.txt',
  'votacao_candidato_munzona_2006_PE.txt',
  'votacao_candidato_munzona_2006_PI.txt',
  'votacao_candidato_munzona_2006_PR.txt',
  'votacao_candidato_munzona_2006_RJ.txt',
  'votacao_candidato_munzona_2006_RN.txt',
  'votacao_candidato_munzona_2006_RO.txt',
  'votacao_candidato_munzona_2006_RR.txt',
  'votacao_candidato_munzona_2006_RS.txt',
  'votacao_candidato_munzona_2006_SC.txt',
  'votacao_candidato_munzona_2006_SE.txt',
  'votacao_candidato_munzona_2006_SP.txt',
  'votacao_candidato_munzona_2006_TO.txt')

# Criacao de data frame vazio para acumular os registros de cada um dos arquivos acima
el_2006_uf_all <- data.frame(matrix(NA, nrow=0, ncol=29))

for (file in el_2006_uf_files) {
  print(file)
  el_2006_uf_filename <- paste(el_2006_basepath, file, sep='/')
  el_2006_uf = read.csv(el_2006_uf_filename, header=FALSE, sep=';')
  
  el_2006_uf_all <- rbind(el_2006_uf_all, el_2006_uf)
}

el_2002_uf_ac[el_2002_uf_ac$V16 == 'DEPUTADO FEDERAL',]

# Apenas votacoes de DEPUTADO FEDERAL
el_2006_uf_df <- el_2006_uf_all[el_2006_uf_all$V16 == 'DEPUTADO FEDERAL',]

# Total de votos por candidato e por turno
el_2006_uf_df_votos <- aggregate(el_2006_uf_df$V29, by=list(el_2006_uf_df$V24, el_2006_uf_df$V4), FUN=sum)

# Novos nomes para as 3 colunas do data frame
names(el_2006_uf_df_votos) <- c('Partido', 'Turno', 'Votos')

# Foca no 1o turno
el_2006_uf_df_votos_turno1 <- el_2006_uf_df_votos[el_2006_uf_df_votos$Turno == 1, ]

# Calculo do percentual de votos de cada candidato do 1o turno
el_2006_uf_df_votos_turno1$VotosPercentual <- el_2006_uf_df_votos_turno1$Votos / sum(el_2006_uf_df_votos_turno1$Votos)

# So PT nos interessa
el_2006_uf_df_votos_turno1_pt <- el_2006_uf_df_votos_turno1[el_2006_uf_df_votos_turno1$Partido == 'PT',]

# Acrescenta coluna Ano
el_2006_uf_df_votos_turno1_pt$Ano <- 2006

# Acrescenta coluna Cargo
el_2006_uf_df_votos_turno1_pt$Cargo <- 'DEPUTADO FEDERAL (PT)'

# RENDERIZANDO A TABELA

el_2002_2006_pr_df_pt <- rbind(el_2002_pr_votos_turno1_lula[ c('Ano','Cargo','Votos','VotosPercentual')], 
      el_2006_pr_votos_turno1_lula[ c('Ano','Cargo','Votos','VotosPercentual')], 
      el_2002_uf_df_votos_turno1_pt[ c('Ano','Cargo','Votos','VotosPercentual')], 
      el_2006_uf_df_votos_turno1_pt[ c('Ano','Cargo','Votos','VotosPercentual')])

pandoc.table(el_2002_2006_pr_df_pt)






