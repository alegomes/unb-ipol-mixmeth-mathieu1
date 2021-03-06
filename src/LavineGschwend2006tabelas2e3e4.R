############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 2 do artigo Lavine & Gschwend (2006)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

library(pander)
library(readr)
library(foreign)
library(car)
library(glue)

# Arg 'concern' = Interest in the Campaign
#   > levels(anes1992$V923101)
#     [1] "VERY MUCH INTERESTED" 
#     [2] "SOMEWHAT INTERESTED"  
#     [3] "NOT MUCH INTERESTED"

run_regression <- function(concern) {
  
  ############################################################
  # NUMBER OF VALID POLICY RESPONSES
  ############################################################
  
  individual_attitude_on_government_spending <- anes1992full$V923701
  individual_attitude_on_defence_spending <- anes1992full$V923707
  individual_attitude_on_job_assurance <- anes1992full$V923718
  individual_attitude_on_abortion <- anes1992full$V923732
  individual_attitude_on_gov_support_blacks_position <- anes1992full$V923729 # ou 923731?
  individual_attitude_on_women_rights <- anes1992full$V923801
  individual_attitude_on_laws_protecting_homosexuals <- anes1992full$V925923 # ou 925924?
  individual_attitude_on_homosexuals_in_the_army <- anes1992full$V925925 # ou 925926?
  individual_attitude_on_gay_adoption <- anes1992full$V925927 # ou 925928?
  individual_attitude_on_government_integration_of_schools <- anes1992full$V925931 # ou 925932?
  individual_attitude_on_capital_punishment <- rnorm(nrow(anes1992full), mean=700, sd=23) # [?] De onde posso tirar?
  individual_attitude_on_preferential_of_blacks <- anes1992full$V925935 # ou 925936?
  individual_attitude_on_school_prayer <- anes1992full$V925945 # ou 925946?
  individual_attitude_on_black_student_quotas <- anes1992full$V925947 # ou 925948?
  
  policy_issues_responses <- data.frame(
      government_spending=individual_attitude_on_government_spending, 
      defence_spending=individual_attitude_on_defence_spending, 
      job_assurance=individual_attitude_on_job_assurance , 
      abortion=individual_attitude_on_abortion, 
      gov_support_blacks_position =individual_attitude_on_gov_support_blacks_position , 
      women_rights=individual_attitude_on_women_rights, 
      laws_protecting_homosexuals=individual_attitude_on_laws_protecting_homosexuals, 
      homosexuals_in_the_army =individual_attitude_on_homosexuals_in_the_army , 
      gay_adoption=individual_attitude_on_gay_adoption, 
      government_integration_of_schools=individual_attitude_on_government_integration_of_schools, 
      capital_punishment=individual_attitude_on_capital_punishment, 
      preferential_of_blacks=individual_attitude_on_preferential_of_blacks, 
      school_prayer=individual_attitude_on_school_prayer, 
      black_student_quotas=individual_attitude_on_black_student_quotas)

  
  anes1992full$num_of_policy_issues_responses <- 
    apply(policy_issues_responses, 1, function(x) { sum(!is.na(x))})
  
  # Respondents who failed to answer at least half of the issue items 
  # in a given election year were excluded from all analyses. 
  
  anes1992 <- anes1992full[anes1992full$num_of_policy_issues_responses > 6,]
  
  # Filtro para as tabelas
  anes1992 <- anes1992[anes1992$V923101 %in% levels(anes1992$V923101)[concern],]
  
  x15_number_of_valid_policy_responses <- anes1992$number_of_policy_responses
  
  
  ############################################################
  # CANDIDATE EVALUATION
  ############################################################
  # Estimates of the effects on summary candidate evaluation 
  # (the thermometer score for the Republican candidate (Bush) 
  # minus the thermometer score for the Democratic candidate (Clinton), 
  # recoded to a 0–1 scale) Pg 150
  feeling_thermometer_republican_candidate <- anes1992$V923305
  feeling_thermometer_democratic_candidate <- anes1992$V923306
  y_candidate_evaluation <- (as.numeric(feeling_thermometer_republican_candidate)/100 - 
                            as.numeric(feeling_thermometer_democratic_candidate)/100)
  
  ############################################################
  # ISSUE PROXIMITY
  ############################################################
  
  # A single issue proximity score was constructed for each 
  # respondent in each election year by averaging all issues 
  # for which the respondent provided a valid response. 
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
  #   Di is the mean perception of the Democratic candidate’s 
  #      (Bush) position on issue i, 
  #   Ri is the mean perception of the Republican candidate’s 
  #      (Clinton) position on issue i, and 
  #   nj is the number of valid policy responses provided by 
  #      voter j.
  #
  #      [?] 'policy responses' sao as respostas para 
  #           as 4 policy issues usadas na composicao do 
  #           indicador ou inclui alguma outra coisa?
  #      [?] O que e uma *valid* policy response?
  #
  # Issue proximity was coded in all election years such 
  # that higher scores represented greater voter issue 
  # similarity with the Republican candidate.
  #
  
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
  voter_valid_policy_responses <- 
    data.frame(gov_spending=gov_spending_voter,
               defense_spending=defense_spending_voter,
               job_assurance=job_assurance_voter,
               abortion=abortion_voter)

  # Matriz com o posicionamento do respondente para cada uma das 4 policy issues 
  voter_positions <- 
    data.frame(gov_spending=as.numeric(levels(gov_spending_voter)[as.integer(gov_spending_voter)]), 
               defense_spending=as.numeric(levels(defense_spending_voter)[as.integer(defense_spending_voter)]), 
               job_assurance=as.numeric(levels(job_assurance_voter)[as.integer(job_assurance_voter)]), 
               abortion=as.numeric(levels(abortion_voter)[as.integer(abortion_voter)]))

  # Media da percepcao do candidato democrata para cada uma das 4 policy issues
  democratic_perception_mean <-
    c(gov_spending=mean(as.numeric(levels(gov_spending_democratic)[as.integer(gov_spending_democratic)]),na.rm=TRUE), 
                        defense_spending=mean(as.numeric(levels(defense_spending_democratic)[as.integer(defense_spending_democratic)]),na.rm=TRUE), 
                        job_assurance=mean(as.numeric(levels(job_assurance_democratic)[as.integer(job_assurance_democratic)]),na.rm=TRUE), 
                        abortion=mean(as.numeric(levels(abortion_democratic)[as.integer(abortion_democratic)]),na.rm=TRUE))

  republican_perception_mean <- 
    c(gov_spending=mean(as.numeric(levels(gov_spending_republican)[as.integer(gov_spending_republican)]),na.rm=TRUE), 
                        defense_spending=mean(as.numeric(levels(defense_spending_republican)[as.integer(defense_spending_republican)]),na.rm=TRUE), 
                        job_assurance=mean(as.numeric(levels(job_assurance_republican)[as.integer(job_assurance_republican)]),na.rm=TRUE), 
                        abortion=mean(as.numeric(levels(abortion_republican)[as.integer(abortion_republican)]),na.rm=TRUE))
  # [TODO] Refatorar
  issue_proximity <- NULL
  for (j in 1:nrow(anes1992)) { # For each responder
    
    scores <- 0
    number_of_valid_policy_responses <- 
      length(which(!is.na(voter_valid_policy_responses[j,])))
    
    for(i in 1:4) { # For each policy issue
      scores <- scores + 
                abs(voter_positions[j,i] - democratic_perception_mean[i]) - 
                abs(voter_positions[j,i]-republican_perception_mean[i])
    }
    issue_proximity <- rbind(issue_proximity, scores/number_of_valid_policy_responses)
  }
  
  x1_issue_proximity <- issue_proximity
  
  ############################################################
  # PARTY IDENTIFICATION
  ############################################################
  
  # [?] Nao esta claro de onde veio a informacao de 'party identification'
  #
  # Vamos considerar que a identificacao partidaria do sujeito tem a ver 
  # com a simpatia que ele tem com cada partido. Aquele que ganhar maior
  # nota, vence.
  #
  feeling_thermometer_democratic_party <- anes1992$V923317
  feeling_thermometer_republican_party <- anes1992$V923318
  
  x2_party_identification <- ifelse(as.numeric(feeling_thermometer_republican_party) < as.numeric(feeling_thermometer_democratic_party), "D", 
                                    ifelse(as.numeric(feeling_thermometer_republican_party) > as.numeric(feeling_thermometer_democratic_party), "R","?"))
  
  ############################################################
  # CHARACTER ASSESSMENT
  ############################################################
  
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
                                (num_of_democratic_positive_comments + num_of_republican_negative_comments))
  
  ############################################################
  # FEMALE
  ############################################################
  
  # Also included in the OLS regressions are 
  # sex (female 􏰃 1; male 􏰃 0),
  # race (white 􏰃 1; otherwise 􏰃 0), and
  # the number of policy issues for which 
  # the respondent provided a valid opinion. Pg 150
  
  # sex (female 􏰃 1; male 􏰃 0),
  anes1992$gender[as.character(anes1992$V924201) == 'FEMALE'] <- 1
  anes1992$gender[as.character(anes1992$V924201) == 'MALE'] <- 0
  x4_female <- anes1992$gender
  
  ############################################################
  # WHITE
  ############################################################
  
  # race (white 􏰃 1; otherwise 􏰃 0), and
  anes1992$race[anes1992$V924202 == 'WHITE'] <- 1
  anes1992$race[anes1992$V924202 != 'WHITE'] <- 0
  x5_white <- anes1992$race
  
  ############################################################
  # EDUCATION
  ############################################################
  
  # Summary: R's Education
  # [TODO] Como ele codificou?!
  x6_education <- anes1992$V923908
  
  ############################################################
  # IDEOLOGICAL THINKING
  ############################################################
  
  # To facilitate a comparison of the coefficients within and between analyses, 
  # all variables were recoded to a 0 to 1 scale. 
  #
  # Moreover, to ease the interpretation of key interactions and to reduce 
  # multicollinearity between individual and cross-product terms, all 
  # variables involved in interaction terms (i.e., ideological thinking, 
  # political knowledge, issue proximity, party identification, perceptions 
  # of candidate character) were centred about their means.
  # 
  #    [?] O que esse ultimo paragrafo quer dizer?
  # 
  
  # Three types of items were included in each election year to assess the 
  # extent to which respondents judged political stimuli in ideological 
  # (liberal–conservative) terms:
  # (1) self-identification as liberal or conservative (versus moderate or no identification); 
  # (2) feeling close to the consistent ideological group; and 
  # (3) consistency between 
  #       ideological identification 
  #     on one hand, and 
  #       party identification, 
  #       feelings towards ideological groups, and 
  #       individual policy attitudes 
  #     on the other 
  # (all policy items included in the ANES for a given year were used 
  # in the assessment of ideological thinking; see the Appendix for 
  # a listing of issues and variable numbers). 
  #
  
  # Ideological Placement
  self_identification <- anes1992$V923509
  
  # Group R feels close to - Liberals/Conservatives
  feeling_close <- data.frame(anes1992$V926203, anes1992$V926211)
  
  # [?] Como calcular esses indicadores de consistencia.
  
  # Footnote 29. Respondents’ ideological self-identifications were considered 
  # consistent with their partisan identifications and their policy attitudes 
  # if their scores were on the same side of the 7-point scales for both items 
  # (i.e., 1, 2 or 3 for liberal/Democrat, and 5, 6 or 7 for conservative/Republican).
  
  # Laco condicional ineficiente em prol da clareza
  consistency_with_party_identification <- 
      ifelse(is.na(x2_party_identification) | is.na(self_identification), FALSE,
      ifelse(x2_party_identification == 'D' & as.numeric(self_identification) < 4, TRUE, 
      ifelse(x2_party_identification == 'D' & as.numeric(self_identification) > 4, FALSE, 
      ifelse(x2_party_identification == 'D' & as.numeric(self_identification) == 4, FALSE,
      ifelse(x2_party_identification == 'R' & as.numeric(self_identification) < 4, FALSE, 
      ifelse(x2_party_identification == 'R' & as.numeric(self_identification) > 4, TRUE, 
      ifelse(x2_party_identification == 'R' & as.numeric(self_identification) == 4, FALSE, FALSE)))))))
  
  
  # Footnote 29. Respondents felt close to the consistent ideological group if liberals 
  # responded as ‘feeling close to’ liberals but not conservatives, 
  # and vice versa for conservatives.
  
  closeness <- data.frame(liberals=anes1992$V926203, 
                          conservatives=anes1992$V926211)
  
  # [TODO] Buscar forma mais R-onica de fazer isto
  closeness$liberals <- ifelse(closeness$liberals == "Mentioned/marked", TRUE, FALSE)
  closeness$conservatives <- ifelse(closeness$conservatives == "Mentioned/marked", TRUE, FALSE)
  
  consistency_with_feelings_towards_groups <- 
    ifelse(as.numeric(self_identification) == 4, FALSE,
    ifelse(as.numeric(self_identification) < 4 & closeness$liberals, TRUE,
    ifelse(as.numeric(self_identification) < 4 & closeness$conservatives, FALSE, 
    ifelse(as.numeric(self_identification) > 4 & closeness$liberals, FALSE, 
    ifelse(as.numeric(self_identification) > 4 & closeness$conservatives, TRUE, FALSE)))))
  
  # [?] Como medir consistencia numa escala numerica e nao booleana?
  # consistency_with_individual_attitudes <- anes1992$V
  
  
  # ideological self-identifications were considered consistent with 
  # ideological feelings if respondents felt warmer towards the ideological 
  # group to which they identified (i.e., for liberals, if the feeling 
  # thermometer score for the group ‘liberals’ was higher than the feeling 
  # thermometer score for the group ‘conservatives’, and vice versa for conservatives).
  # ??????
  
  # Individual scores were computed by summing the number of 
  # ideologically correct responses.
  # 
  # [?] O que sao "ideologically correct responses"?
  #     Correct com base em que?
  
  # Nada do que viermos a calcular com tantas incerteza sera melhor 
  # do que uma listagem normal randomica de numeros :-)
  x7_ideological_thinking <- rnorm(nrow(anes1992), mean=1000, sd=10)
  
  
  ############################################################
  # POLITICAL KNOWLEDGE
  ############################################################
  
  # Each of the ANES surveys included a battery of objective knowledge questions, 
  # pertaining to jobs held by well-known political figures (such as Margaret Thatcher, 
  # Newt Gingrich, Yasser Arafat), the responsibilities of each branch of government 
  # (for example, who nominates judges to the federal courts), and party control 
  # of Congress (for a listing of the items, see the Appendix). 
  #
  # All political knowledge items were scored 1 if correct and 0 if incorrect (or ‘don’t know’)
  
  # Does R know job/office Dan Quayle holds?	
  political_knowledge_dan_quayle <- 
    unlist(recode(lapply(anes1992$V925916, as.character), 
                  "'Correctly identifies Quayle'=1; 'Identification is incomplete or wrong'=0;"),
                  use.names=FALSE)
  
  # Does R know job/office William Rehnquist holds?	
  political_knowledge_william_rehnquist <- 
    unlist(recode(lapply(anes1992$V925917, as.character), 
                  "'Correctly identifies Rehnquist'=1; 'Identification is incomplete or wrong'=0;"),
                  use.names=FALSE)
  
  # Does R know job/office Boris Yeltsin holds?	
  political_knowledge_boris_yeltsin <-
    unlist(recode(lapply(anes1992$V925918, as.character), 
                  "'Correctly identifies Yeltsin'=1; 'Identification is incomplete or wrong'=0;"),
                  use.names=FALSE)
  
  # Does R know job/office Tom Foley holds?	
  political_knowledge_tom_foley <- 
    unlist(recode(lapply(anes1992$V925919, as.character), 
                  "'Correctly identifies Foley'=1; 'Identification is incomplete or wrong'=0;"),
                  use.names=FALSE)
  
  
  # Who has the final responsibility to decide the constitutionality of law?	
  political_knowledge_constitutionality <- 
    unlist(recode(lapply(anes1992$V925920, as.character), 
                  "'PRESIDENT'=0; 'CONGRESS'=0; 'SUPREME COURT'=1"),use.names=FALSE)
  
  # Who nominates judges to the federal courts?	
  political_knowledge_federal_court <- 
    unlist(recode(lapply(anes1992$V925921, as.character), 
                  "'PRESIDENT'=1; 'CONGRESS'=0; 'SUPREME COURT'=0"),use.names=FALSE)
  
  # Does R know which party had most members in the US House before the election?	
  political_knowledge_ushouse_most_member <- 
    unlist(recode(lapply(anes1992$V925951, as.character), 
                  "'REPUBLICANS'=0; 'DEMOCRATS'=1;"),use.names=FALSE)
  
  # Does R know which party had most members in the Senate before the election?	
  political_knowledge_senate_most_member <- 
    unlist(recode(lapply(anes1992$V925952, as.character), 
                  "'REPUBLICANS'=0; 'DEMOCRATS'=1;"),use.names=FALSE)
  
  political_knowledge_dan_quayle[is.na(political_knowledge_dan_quayle)] <- 0
  political_knowledge_william_rehnquist[is.na(political_knowledge_william_rehnquist)] <- 0
  political_knowledge_boris_yeltsin[is.na(political_knowledge_boris_yeltsin)] <- 0
  political_knowledge_tom_foley[is.na(political_knowledge_tom_foley)] <- 0
  
  political_knowledge_constitutionality[is.na(political_knowledge_constitutionality)] <- 0
  political_knowledge_federal_court[is.na(political_knowledge_federal_court)] <- 0
  political_knowledge_ushouse_most_member[is.na(political_knowledge_ushouse_most_member)] <- 0
  political_knowledge_senate_most_member[is.na(political_knowledge_senate_most_member)] <- 0
  
  political_knowledge <- data.frame(dan_quayle=political_knowledge_dan_quayle,
                                    william_rehnquist=political_knowledge_william_rehnquist,
                                    boris_yeltsin=political_knowledge_boris_yeltsin,
                                    tom_foley=political_knowledge_tom_foley,
                                    constitutionality=political_knowledge_constitutionality,
                                    federal_court=political_knowledge_federal_court,
                                    ushouse_most_member=political_knowledge_ushouse_most_member,
                                    senate_most_member=political_knowledge_senate_most_member)
  
  # [?] Tem que ser na escala 0-1?
  x8_political_knowledge <- apply(political_knowledge, 1, function(x){ sum(as.numeric(x))})
  
  # To test the switching mechanism hypothesis, six interaction terms were 
  # constructed. In the first set of terms, 
  #   ideological thinking scores 
  # were multiplied by 
  #   issue proximity scores, 
  #   party identification scores and 
  #   character assessment scores, 
  # respectively.
  
  ############################################################
  # IDEOLOGICAL THINKING x CHARACTER ASSESMENT
  ############################################################
  # x9_ideological_thinking_character_assessment <- rnorm(2485, mean=1000, sd=10)
  
  ############################################################
  # IDEOLOGICAL THINKING x ISSUE PROXIMITY
  ############################################################
  # x10_ideological_thinking_issue_proximity <- rnorm(2485, mean=900, sd=10)
  
  ############################################################
  # IDEOLOGICAL THINKING x PARTY IDENTIFICATION
  ############################################################
  # x11_ideological_thinking_party_identification <- rnorm(2485, mean=1100, sd=10)
  
  
  # To control for the effects of political knowledge, we constructed 
  # a second set of interaction terms in which 
  #   issue proximity, 
  #   party identification and 
  #   character assessment 
  # were each multiplied by 
  #   political knowledge scores.
  
  ############################################################
  # POLITICAL KNOWLEDGE x CHARACTER ASSESMEN
  ############################################################
  # x12_political_knowledge_character_assessment <- rnorm(100, mean=35, sd=10)
  
  ############################################################
  # POLITICAL KNOWLEDGE x ISSUE PROXIMITY
  ############################################################
  # x13_political_knowledge_issue_proximity <- rnorm(100, mean=30, sd=10)
  
  ############################################################
  # POLITICAL KNOWLEDGE x PARTY IDENTIFICATION
  ############################################################
  # x14_political_knowledge_party_identification <- rnorm(100, mean=25, sd=10)
  
  
  model <- lm(y_candidate_evaluation~ x1_issue_proximity+
                x2_party_identification+
                x3_character_assessment+
                x4_female+
                x5_white+
                x6_education+
                x7_ideological_thinking+
                x8_political_knowledge+
                x7_ideological_thinking*x3_character_assessment+
                x7_ideological_thinking*x1_issue_proximity+ 
                x7_ideological_thinking*x2_party_identification+ 
                x8_political_knowledge*x3_character_assessment+ 
                x8_political_knowledge*x1_issue_proximity+
                x8_political_knowledge*x2_party_identification)
  
  return(model)
  
}

filepath <- '/Users/alegomes/GDrive/2018/unb/ipol/disc\ métodos\ mistos/provas/1.\ Mathieu/data/anes1992por/anes1992.POR'
anes1992full <- read.spss(filepath, to.data.frame=TRUE)

tabela2 <- run_regression(c(1,2,3))
summary(tabela2)

tabela3 <- run_regression(c(3))
summary(tabela3)

tabela4 <- run_regression(c(1))
summary(tabela4)


# TABELA 2


col1 <- c('Issue Proximity', 
               'Party Identification', 
               'Character Assessment',
               'Female', 
               'White', 
               'Education', 
               'Ideological Thinking', 
               'Political Knowledge', 
               'Ideological Thinking 􏰆x Character Assessment', 'Ideological Thinking 􏰆x Issue Proximity',
               'Ideological Thinking 􏰆x Party Identification',
               'Political Knowledge 􏰆x Character Assessment',
               'Political Knowledge 􏰆x Issue Proximity',
               'Political Knowledge 􏰆x Party Identification',
               'Number of Valid Policy Responses', 
               'Constant', 
               'R2', 
               'N')

# col2 <- c(glue({round(model$coefficients, digit=3)['x1_issue_proximity']} ({round(model$coefficients, digit=3)['x1_issue_proximity']})) 
#   
#         )
# 
# table <- data.frame(variaveis, )


# Tabelas 3 e 4 :
# 923101 Interest in the Campaign


