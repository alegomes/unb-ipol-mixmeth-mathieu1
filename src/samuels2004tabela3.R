############################################################
# Métodos Mistos - IPol/UnB - 2º/2018
# Prof. Dr. Mathieu Turgeon
# Reproducao da TABELA 3 do artigo Samuels (2004)
# Alunos: Alexandre Gomes e Beatriz Franco
############################################################

identificacao_partidaria <- sample(rep(as.factor(c('PT','PSDB', 'PMDB', 'PFL', 'Outro', 'Nenhuma')),1000),1000)

cat_emp_trabalhador_rural <- sample(rep(c(0,1),1000),1000)
cat_emp_pequeno_burgues <- sample(rep(c(0,1),1000),1000)
cat_emp_dona_casa <- sample(rep(c(0,1),1000),1000)
cat_emp_setor_informal <- sample(rep(c(0,1),1000),1000)
cat_emp_profissional_liberal <- sample(rep(c(0,1),1000),1000)
cat_emp_militar_policial <- sample(rep(c(0,1),1000),1000)
cat_emp_maodeobra_especializada <- sample(rep(c(0,1),1000),1000)
cat_emp_maodeobra_nao_especalizada <- sample(rep(c(0,1),1000),1000)
cat_emp_aposentado <- sample(rep(c(0,1),1000),1000)
cat_emp_estudante <- sample(rep(c(0,1),1000),1000)
cat_emp_desempregado <- sample(rep(c(0,1),1000),1000)

preferencias <- factor(c(1,2,3,4,5,6), labels=c('pessimo', 'ruim', 'regular pra ruim', 'regular pra bom', 'bom', 'otimo'), ordered=TRUE)

av_gov_fhc <- sample(rep(preferencias,1000),1000)
opiniao_sobre_lula <- sample(rep(factor(0:12, ordered=T),1000),1000)
ideologia_esquerda_direita <- sample(rep(factor(c(0:10,66), ordered=T),1000),1000)

logit <- glm(identificacao_partidaria ~  cat_emp_trabalhador_rural + 
                                         cat_emp_pequeno_burgues +
                                         cat_emp_dona_casa +
                                         cat_emp_setor_informal + 
                                         cat_emp_profissional_liberal + 
                                         cat_emp_militar_policial +
                                         cat_emp_maodeobra_especializada + 
                                         cat_emp_maodeobra_nao_especalizada +
                                         cat_emp_aposentado +
                                         cat_emp_estudante +
                                         cat_emp_desempregado + 
                                         av_gov_fhc +
                                         opiniao_sobre_lula +
                                         ideologia_esquerda_direita
              ,family="binomial")

summary(logit)
