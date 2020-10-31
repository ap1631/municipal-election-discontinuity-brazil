# Header ----------------------------------------------------------
# DOWNLOAD ELECTORAL DATA

#### Libraries
library(electionsBR)
library(tidyverse)

#### Sequence years
years <- seq(2000,2016, by=4)
years_special <- seq(2000,2012, by=4)


#### All data types

# candidate_fed() --> candidate information, including profession and education
# candidate_local()
# details_mun_zone_fed() --> voter turnout
# details_mun_zone_local()
# legend_fed() --> 
# legend_local()
# legislative_mun_vote()
# legislative_state_vote()
# parties_br()
# party_mun_zone_fed()
# party_mun_zone_local()
# president_mun_vote()
# president_state_vote()
# seats_fed()
# seats_local()
# uf_br()
# vote_mun_zone_fed()
# vote_mun_zone_local()
# voter_affiliation() --> microdata, per party per state
# voter_profile()




# Voting Turnout ----------------------------------------------------------
# Download all voting turnout data per city

#### Create catch-all
vote_turnout <- tibble()

#### Loop
for(i in years){
  dat <- details_mun_zone_local(i)
  dat <- dat %>% select(ANO_ELEICAO, NUM_TURNO,
                DESCRICAO_ELEICAO,
                SIGLA_UF, CODIGO_MUNICIPIO,
                CODIGO_CARGO, DESCRICAO_CARGO, QTD_APTOS_TOT,
                QTD_COMPARECIMENTO, QTD_ABSTENCOES,  QTD_VOTOS_NOMINAIS, QTD_VOTOS_BRANCOS,
                QTD_VOTOS_NULOS, QTD_VOTOS_LEGENDA)
  vote_turnout <- rbind(vote_turnout, dat)
}

#### Save downloaded data
save(vote_turnout, file="vote_turnout_2000_2016.rds")




# Voting Results ----------------------------------------------------------
## Download all voting results data per candidate

#### Create catch-all
vote_results <- tibble()

#### Loop
for(i in years){
  dat <- vote_mun_zone_local(i)
  dat <- dat %>% select (ANO_ELEICAO, NUM_TURNO, DESCRICAO_ELEICAO,
                         SIGLA_UF, SIGLA_UE, CODIGO_MUNICIPIO, NOME_MUNICIPIO, NUMERO_ZONA, CODIGO_CARGO,
                         NUMERO_CAND, NOME_CANDIDATO, CODIGO_SIT_CAND_TOT, DESC_SIT_CAND_TOT, 
                         NUMERO_PARTIDO, SIGLA_PARTIDO,  NOME_PARTIDO,  
                         SEQUENCIAL_LEGENDA, NOME_COLIGACAO, COMPOSICAO_LEGENDA, TOTAL_VOTOS
  )
  vote_results <- rbind(vote_results, dat)
}

#### Save downloaded data
save(vote_results, file="vote_results_2000_2016.rds")


# Voter Profile ----------------------------------------------------------

## Inspect structure of dataframes

####Download data
profile_2000 <- voter_profile(2000)
profile_2004 <- voter_profile(2004)
profile_2008 <- voter_profile(2008)
profile_2012 <- voter_profile(2012) ## Tem que arrumar todos os nomes de coluna desse ano!
profile_2016 <- voter_profile(2016)

#### Get col names
colnames(profile_2000) ## 2004 and 2008 are the same structure
# [1] "PERIODO"                 "UF"                     
# [3] "MUNICIPIO"               "COD_MUNICIPIO_TSE"      
# [5] "NR_ZONA"                 "SEXO"                   
# [7] "FAIXA_ETARIA"            "GRAU_DE_ESCOLARIDADE"   
# [9] "QTD_ELEITORES_NO_PERFIL"

colnames(profile_2012)
# [1] "PERIODO"                 "UF"                     
# [3] "MUNICIPIO"               "COD_MUNICIPIO_TSE"      
# [5] "NR_ZONA"                 "SEXO"                   
# [7] "FAIXA_ETARIA"            "GRAU_DE_ESCOLARIDADE"   
# [9] "QTD_ELEITORES_NO_PERFIL" NA                       
# [11] NA                        NA                       
# [13] NA                        NA                       
# [15] NA                        NA                       
# [17] NA                        NA                       
# [19] NA                        NA                       
# [21] NA   

colnames(profile_2016)
# [1] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9"

#### Rename NA columns COMEÇAR AQUI PELAS NOMEADAS!
# profile_2012$__ <- profile_2012[,__] 

#### Filter columns
# profile_2012 <- 
#   profile_2012 %>% 
#   select(PERIODO, UF, 
#          MUNICIPIO, COD_MUNICIPIO_TSE, 
#          NR_ZONA, SEXO,
#          FAIXA_ETARIA, GRAU_DE_ESCOLARIDADE,
#          QTD_ELEITORES_NO_PERFIL)
# profile_2016 <- voter_profile(2016)

# voter_profile <- rbind(profile_2000, profile_2004, profile_2008, profile_2012, profile_2016)

# voter_profile %>% group_by(PERIODO) %>% count()

#### Save downloaded data
# save(voter_profile, file="voter_profile_2000_2016.rds")



## Inspect structure of dataframes
#### Downloadt tests
#### Get colnames
#### Ger relevant info
## Download all 
#### Create catch-all
#### Loop
#### Save downloaded data

## Download all
#### Create catch-all
#### Loop
#### Save downloaded data


# Voter Affiliation ----------------------------------------------------------

# ## Inspect structure of dataframes
# #### Downloadt tests
# voter_affiliation <- voter_affiliation("pt", "rj")
# voter_affiliation_2000 <- voter_affiliation(2000,uf="rio de janeiro")
# voter_affiliation_2016 <- voter_affiliation(2016)
# #### Get colnames
# #### Ger relevant info
# 
# ## Download all voter affiliation data
# #### Create catch-all
# voter_affiliation <- tibble()
# #### Loop
# for(i in years){
#   dat <- voter_affiliation(i)
#   voter_affiliation <- rbind(voter_affiliation, dat)
# }
# save(voter_affiliation, file="voter_affiliation_2000_2016.rds")

# Candidate Profile ----------------------------------------------------------

candidate_2012

# df <- candidate_profile %>% filter(CODIGO_CARGO==11) %>% group_by(DESCRICAO_OCUPACAO) %>% count() 

# candidate_profile %>% filter(CODIGO_CARGO==11) %>%  group_by(DESCRICAO_OCUPACAO) %>% count() %>% filter(n>1000)
# candidate_profile %>% filter(CODIGO_CARGO==11) %>%  group_by(DESCRICAO_GRAU_INSTRUCAO) %>% count() 
# 
# x <- candidate_profile %>% 
#   mutate("eleito" = ifelse(DESC_SIT_TOT_TURNO=="ELEITO",1,0), 
#                              "superior" = ifelse(
#                                DESCRICAO_GRAU_INSTRUCAO=='2º GRAU INCOMPLETO'|
#                                  DESCRICAO_GRAU_INSTRUCAO=='2º GRAU COMPLETO'|
#                                  DESCRICAO_GRAU_INSTRUCAO=='SUPERIOR COMPLETO'|
#                                  DESCRICAO_GRAU_INSTRUCAO=='SUPERIOR INCOMPLETO', 1, 0)) %>% 
#   filter(CODIGO_CARGO==11)
#   
# ols <- lm(eleito ~ superior + factor(CODIGO_SEXO) + factor(ANO_ELEICAO), data=x)
# summary(ols)

## Download all candidate profile data
#### Create catch-all
candidate_profile <- tibble()
#### Loop
for(i in years){
  dat <- candidate_local(i)
  candidate_profile <- rbind(candidate_profile, dat)
}
save(candidate_profile, file="candidate_profile_2000_2016.rds")


eleitores <- voter_profile(2000)