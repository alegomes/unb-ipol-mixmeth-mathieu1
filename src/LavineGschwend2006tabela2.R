############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 2 do artigo Lavine & Gschwend (2006)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(readr)
library(foreign)

filepath <- '/Users/alegomes/GDrive/2018/unb/ipol/disc\ métodos\ mistos/provas/1.\ Mathieu/data/anes1992por/anes1992.POR'
anes1992 <- read.spss(filepath, to.data.frame=TRUE)

#########################################################################
# CANDIDATE EVALUATION
#########################################################################

# Estimates of the effects on summary candidate evaluation 
# (the thermometer score for the Republican candidate (Bush) 
# minus the thermometer score for the Democratic candidate (Clinton), 
# recoded to a 0–1 scale) Pg 150
feeling_thermometer_republican_candidate <- anes1992$V923305
feeling_thermometer_democratic_candidate <- anes1992$V923306
y_candidate_evaluation <- (as.numeric(feeling_thermometer_republican_candidate)/100 - 
                           as.numeric(feeling_thermometer_democratic_candidate)/100)[1:100]

#########################################################################
# ISSUE PROXIMITY
#########################################################################

# A single issue proximity score was constructed for each respondent 
# in each election year by averaging all issues for which the respondent
# provided a valid response. 
#
# This resulted in (…) four (policy) issues in 1992
#   - Government services/spending*
#   - Defence spending*
#   - Job assurance*
#   - Abortion*
#
# The formula used to construct issue proximity was:
#   (∑|Vij - Di| - |Vij - Ri| )/nj
#
#      [?] As barras verticais sao o simbolo da operacao 
#          modulo ou deveriam ser colchetes?
#      [?] O somatorio se aplica apenas ao primeiro termo 
#          (Vij - Di) ou a tudo o que esta entre os parenteses?
# where 
#   Vij is voter j’s position on issue i, 
#   Di is the mean perception of the Democratic candidate’s (Bush) position on issue i, 
#   Ri is the mean perception of the Republican candidate’s (Clinton) position on issue i, and 
#   nj is the number of valid policy responses provided by voter j.
#
#      [?] 'policy responses' sao as respostas para as 4 policy issues usadas 
#           na composicao do indicador ou inclui alguma outra coisa?
#      [?] O que e uma *valid* policy response?
#
# Issue proximity was coded in all election years such that higher scores 
# represented greater voter issue similarity with the Republican candidate.
#
# TODO Incluir esta critica na selecao das observacoes
# Respondents who failed to answer at least half of the issue items 
# in a given election year were excluded from all analyses. 

gov_spending_voter <- factor(as.numeric(anes1992$V923701))
gov_spending_republican <- factor(as.numeric(anes1992$V923702))
gov_spending_democratic <- factor(as.numeric(anes1992$V923703))

defense_spending_voter <- factor(as.numeric(anes1992$V923707))
defense_spending_republican <- factor(as.numeric(anes1992$V923708))
defense_spending_democratic <- factor(as.numeric(anes1992$V923709))

job_assurance_voter <- factor(as.numeric(anes1992$V923718))
job_assurance_republican <- factor(as.numeric(anes1992$V923719))
job_assurance_democratic <- factor(as.numeric(anes1992$V923720))

abortion_voter <- factor(as.numeric(anes1992$V923732))
abortion_republican <- factor(as.numeric(anes1992$V923733))
abortion_democratic <- factor(as.numeric(anes1992$V923734))

# Consideramos como valid todas as respostas ≠ N/A
voter_valid_policy_responses <- data.frame(gov_spending=gov_spending_voter,
                                           defense_spending=defense_spending_voter,
                                           job_assurance=job_assurance_voter,
                                           abortion=abortion_voter)

# Matriz com o posicionamento do respondente para cada uma das 4 policy issues 
voter_positions <- data.frame(gov_spending=as.numeric(levels(gov_spending_voter)[as.integer(gov_spending_voter)]), 
                              defense_spending=as.numeric(levels(defense_spending_voter)[as.integer(defense_spending_voter)]), 
                              job_assurance=as.numeric(levels(job_assurance_voter)[as.integer(job_assurance_voter)]), 
                              abortion=as.numeric(levels(abortion_voter)[as.integer(abortion_voter)]))

# Media da percepcao do candidato democrata para cada uma das 4 policy issues
democratic_perception_mean <- c(gov_spending=mean(as.numeric(levels(gov_spending_democratic)[as.integer(gov_spending_democratic)]),na.rm=TRUE), 
                                defense_spending=mean(as.numeric(levels(defense_spending_democratic)[as.integer(defense_spending_democratic)]),na.rm=TRUE), 
                                job_assurance=mean(as.numeric(levels(job_assurance_democratic)[as.integer(job_assurance_democratic)]),na.rm=TRUE), 
                                abortion=mean(as.numeric(levels(abortion_democratic)[as.integer(abortion_democratic)]),na.rm=TRUE))

republican_perception_mean <- c(gov_spending=mean(as.numeric(levels(gov_spending_republican)[as.integer(gov_spending_republican)]),na.rm=TRUE), 
                                defense_spending=mean(as.numeric(levels(defense_spending_republican)[as.integer(defense_spending_republican)]),na.rm=TRUE), 
                                job_assurance=mean(as.numeric(levels(job_assurance_republican)[as.integer(job_assurance_republican)]),na.rm=TRUE), 
                                abortion=mean(as.numeric(levels(abortion_republican)[as.integer(abortion_republican)]),na.rm=TRUE))
# TODO Refatorar
issue_proximity <- NULL
for (j in 1:length(anes1992)) { # For each responder
  
  scores <- 0
  number_of_valid_policy_responses <- length(which(!is.na(voter_valid_policy_responses[j,])))
  
  for(i in 1:4) { # For each policy issue
     scores <- scores + abs(voter_positions[j,i] - democratic_perception_mean[i]) - abs(voter_positions[j,i]-republican_perception_mean[i])
  }
  issue_proximity <- rbind(issue_proximity, scores/number_of_valid_policy_responses)
}

x1_issue_proximity <- issue_proximity[1:100]

#########################################################################
# PARTY IDENTIFICATION
#########################################################################

# [?] Nao esta claro de onde veio a informacao de 'party identification'
#
# Vamos considerar que a identificacao partidaria do sujeito tem a ver 
# com a simpatia que ele tem com cada partido. Aquele que ganhar maior
# nota, vence.
#
# TODO Resolver o caso de ambos os termometros serem iguais
#
feeling_thermometer_democratic_party <- anes1992$V923317
feeling_thermometer_republican_party <- anes1992$V923318
x2_party_identification <- ifelse(as.numeric(feeling_thermometer_republican_party) < as.numeric(feeling_thermometer_democratic_party), "D", "R")[1:100]

#########################################################################
# CHARACTER ASSESSMENT
#########################################################################

# In constructing character assessment scores, we used the ‘Experience and Ability’, 
# ‘Leadership Qualities’, and ‘Personal Qualities’, ANES master code categories. 
# (…) we excluded comments not related to candidate qualities, such as those related 
# to issues, parties and groups (e.g., the master code categories ‘Domestic Issues’, 
# ‘Foreign Issues’, ‘Group Connections’ and ‘Government Activity/Philosophy’).
#
# Comparative character assessment scores were constructed by the formula: 
#
# (Pr + Nd) - (Pd + Nr)
#  
# where 
#    Pr and Pd represent the number of positive comments about the 
#       Republican and Democratic candidates, respectively, and 
#    Nr and Nd represent the number of negative comments about the 
#       Republican and Democratic candidates.
#
#    [?] O que se considera como comentarios positivos? 
#        Não esta claro se seriam os apontamentos nas 5 questoes 
#        do tipo 'Reasons would vote for/against ...'. 
#        Vamos considerar que sim.
#       

republican_positive_comments <- data.frame(vote_for_bush1=anes1992$V923110,
                                           vote_for_bush2=anes1992$V923111,
                                           vote_for_bush3=anes1992$V923112,
                                           vote_for_bush4=anes1992$V923113,
                                           vote_for_bush5=anes1992$V923114)

republican_negative_comments <- data.frame(vote_against_bush1=anes1992$V923116,
                                          vote_against_bush2=anes1992$V923117,
                                          vote_against_bush3=anes1992$V923118,
                                          vote_against_bush4=anes1992$V923119,
                                          vote_against_bush5=anes1992$V923120)

democratic_positive_comments <- data.frame(vote_for_clinton1=anes1992$V923122,
                                           vote_for_clinton2=anes1992$V923123,
                                           vote_for_clinton3=anes1992$V923124,
                                           vote_for_clinton4=anes1992$V923125,
                                           vote_for_clinton5=anes1992$V923126)

democratic_negative_comments <- data.frame(vote_against_clinton1=anes1992$V923128,
                                          vote_against_clinton2=anes1992$V923129,
                                          vote_against_clinton3=anes1992$V923130,
                                          vote_against_clinton4=anes1992$V923131,
                                          vote_against_clinton5=anes1992$V923132)

num_of_republican_positive_comments <- apply(republican_positive_comments, 1, function(x){sum(!is.na(x))})
num_of_republican_negative_comments <- apply(republican_negative_comments, 1, function(x){sum(!is.na(x))})
num_of_democratic_positive_comments <- apply(democratic_positive_comments, 1, function(x){sum(!is.na(x))})
num_of_democratic_negative_comments <- apply(democratic_negative_comments, 1, function(x){sum(!is.na(x))})

x3_character_assessment <- ((num_of_republican_positive_comments + num_of_democratic_negative_comments) -
                           (num_of_democratic_positive_comments + num_of_republican_negative_comments))[1:100]

#########################################################################
# 
#########################################################################

# Also included in the OLS regressions are 
# sex (female 􏰃 1; male 􏰃 0),
# race (white 􏰃 1; otherwise 􏰃 0), and
# the number of policy issues for which 
# the respondent provided a valid opinion. Pg 150

# sex (female 􏰃 1; male 􏰃 0),
anes1992$gender[as.character(anes1992$V923089) == 'Female'] <- 1
anes1992$gender[as.character(anes1992$V923089) == 'Male'] <- 0
x4_female <- anes1992$gender[1:100]

#########################################################################
# 
#########################################################################

# race (white 􏰃 1; otherwise 􏰃 0), and
anes1992$race[anes1992$V923085 == 'White'] <- 1
anes1992$race[anes1992$V923085 != 'White'] <- 0
x5_white <- anes1992$race[1:100]

#########################################################################
# 
#########################################################################

# the number of policy issues for which the respondent provided a valid opinion
# TODO Qual eh a variavel?  

#########################################################################
# 
#########################################################################

# TODO Como ele codificou?!
x6_education <- anes1992$V923090[1:100]

#########################################################################
# 
#########################################################################

x7_ideological_thinking <- runif(100)
#########################################################################
# 
#########################################################################
x8_political_knowledge <- runif(100)
#########################################################################
# 
#########################################################################
x9_ideological_thinking_character_assessment <- rnorm(100, mean=50, sd=10)
#########################################################################
# 
#########################################################################
x10_ideological_thinking_issue_proximity <- rnorm(100, mean=45, sd=10)
#########################################################################
# 
#########################################################################
x11_ideological_thinking_party_identification <- rnorm(100, mean=40, sd=10)
#########################################################################
# 
#########################################################################
x12_political_knowledge_character_assessment <- rnorm(100, mean=35, sd=10)
#########################################################################
# 
#########################################################################
x14_political_knowledge_issue_proximity <- rnorm(100, mean=30, sd=10)
#########################################################################
# 
#########################################################################
x13_political_knowledge_party_identification <- rnorm(100, mean=25, sd=10)
#########################################################################
# 
#########################################################################
x15_number_of_valid_policy_responses <- rnorm(100, mean=20, sd=10)

model <- lm(y_candidate_evaluation~ x1_issue_proximity+
                                    x2_party_identification+
                                    x3_character_assessment+
                                    x4_female+
                                    x5_white+
                                    x6_education+
                                    x7_ideological_thinking+
                                    x8_political_knowledge+
                                    x9_ideological_thinking_character_assessment+
                                    x10_ideological_thinking_issue_proximity+
                                    x11_ideological_thinking_party_identification+
                                    x12_political_knowledge_character_assessment+
                                    x13_political_knowledge_party_identification+
                                    x14_political_knowledge_issue_proximity+
                                    x15_number_of_valid_policy_responses)

summary(model)

# Tabelas 3 e 4 :
# 923101 Interest in the Campaign


#attributes(model)

#rnorm(10, mean=50, sd=10)

