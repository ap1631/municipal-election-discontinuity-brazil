# Transform data

load("vote_turnout_2000_2016.rds") ## Voter turnout
load("vote_results_2000_2016.rds") # Result per candidate


# output_healthACS <- readxl::read_xlsx(path="Historico-ACS-MUNICIPIOS-2007-2018.xlsx")
# output_healthAB <- readxl::read_xlsx(path="Historico-AB-MUNICIPIOS-2007-201909.xlsx")
city_codes <- read.csv(url("https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv"))


# Vote Results of General Elections for Mayor  ----------------------------

# Just mayors results
results_mayor <- 
  vote_results %>% 
  filter(CODIGO_CARGO==11)

# Just general elections

## Get codes for general elections
relevant_election_codes <- results_mayor %>% 
  group_by(DESCRICAO_ELEICAO) %>% 
  count() %>% 
  tibble() %>% 
  filter(n>400)
codes <- c(unique(relevant_election_codes$DESCRICAO_ELEICAO))

## Create tibble
results_mayor_general <- tibble()

## Run loop cleaning codes
for (i in codes){
  x<- results_mayor %>% filter(DESCRICAO_ELEICAO==i)
  results_mayor_general <- rbind(results_mayor_general, x)
}

## Save file
# save(results_mayor_general, file="vote_results_mayor_general_2000_2016.rds")
# So we have 103.008 general elections records




# Turnout Results of General Elections for Mayor  ----------------------------

# Just mayors turnout
turnout_mayors <-
  vote_turnout %>% 
  filter(CODIGO_CARGO==11)

# Create tibble

turnout_mayor_general_zones <- tibble()

## Run loop cleaning codes, keeping just general elections
for (i in codes){
  x<- turnout_mayors %>% filter(DESCRICAO_ELEICAO==i)
  turnout_mayor_general_zones <- rbind(turnout_mayor_general, x)
}

## Aggregate at city level
turnout_mayor_general <- turnout_mayor_general_zones %>% 
  group_by(ANO_ELEICAO, NUM_TURNO, SIGLA_UF, CODIGO_MUNICIPIO) %>% 
  summarise_at(vars(QTD_APTOS_TOT, QTD_COMPARECIMENTO, QTD_ABSTENCOES,
                QTD_VOTOS_NOMINAIS, QTD_VOTOS_BRANCOS, QTD_VOTOS_NULOS,
                QTD_VOTOS_LEGENDA), funs(sum))


## Save file
# save(turnout_mayor_general, file="turnout_mayor_general_2000_2016.rds")
# So we have 103.008 general elections records





# Candidates in General Elections for Mayor  ----------------------------

# Count the number of candidates

## Create slice function
first <- function(x) {
  bind_rows(slice(x, 1))
}

## Create datasets per election year
n_candidates_2000 <- 
  results_mayor_general %>% 
  filter(ANO_ELEICAO==2000) %>% 
  group_by(CODIGO_MUNICIPIO, NUMERO_ZONA) %>%
  count() %>% 
  group_by(CODIGO_MUNICIPIO) %>%
  do(first(.)) %>%
  ungroup

n_candidates_2004 <- 
  results_mayor_general %>% 
  filter(ANO_ELEICAO==2004) %>% 
  group_by(CODIGO_MUNICIPIO, NUMERO_ZONA) %>%
  count() %>% 
  group_by(CODIGO_MUNICIPIO) %>%
  do(first(.)) %>%
  ungroup

n_candidates_2008 <- 
  results_mayor_general %>% 
  filter(ANO_ELEICAO==2008) %>% 
  group_by(CODIGO_MUNICIPIO, NUMERO_ZONA) %>%
  count() %>% 
  group_by(CODIGO_MUNICIPIO) %>%
  do(first(.)) %>%
  ungroup

n_candidates_2012 <- 
  results_mayor_general %>% 
  filter(ANO_ELEICAO==2012) %>% 
  group_by(CODIGO_MUNICIPIO, NUMERO_ZONA) %>%
  count() %>% 
  group_by(CODIGO_MUNICIPIO) %>%
  do(first(.)) %>%
  ungroup

n_candidates_2016 <- 
  results_mayor_general %>% 
  filter(ANO_ELEICAO==2016) %>% 
  group_by(CODIGO_MUNICIPIO, NUMERO_ZONA) %>%
  count() %>% 
  group_by(CODIGO_MUNICIPIO) %>%
  do(first(.)) %>%
  ungroup

## Add year
n_candidates_2000$ANO_ELEICAO <- 2000
n_candidates_2004$ANO_ELEICAO <- 2004
n_candidates_2008$ANO_ELEICAO <- 2008
n_candidates_2012$ANO_ELEICAO <- 2012
n_candidates_2016$ANO_ELEICAO <- 2016

## Bind in dataset
n_candidates_cities <- 
  rbind(n_candidates_2000,
        n_candidates_2004,
        n_candidates_2008,
        n_candidates_2012,
        n_candidates_2016) %>% 
  select(-NUMERO_ZONA)

#save(n_candidates_cities, file="n_candidates_cities_2000_2016.rds")









