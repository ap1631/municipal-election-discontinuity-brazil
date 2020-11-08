## CREATE RDDS

# Load Packages -----------------------------------------------------------

library(stargazer)
require(papeR)
library(electionsBR)
library(tidyverse)
require(lubridate)
library(knitr)


# Load data ---------------------------------------------------------------

## Preloaded data
load("vote_results_mayor_general_2000_2016.rds") # Result per candidate just for mayors in general elections 
load("turnout_mayor_general_2000_2016.rds") # turnout of votes per city
load("n_candidates_cities_2000_2016.rds") # N of candidates for mayor in general elections


# Data for quadratic (global strategy) models ----------------------------------------------------------

## Create dataset
vote_turnout_candidates_quadratic <- 
  left_join(turnout_mayor_general, 
            n_candidates_cities, 
            by=c("ANO_ELEICAO", "CODIGO_MUNICIPIO")) %>% 
  #filter(QTD_APTOS_TOT>100000, QTD_APTOS_TOT<300000, NUM_TURNO==1) %>% 
  mutate("D" = ifelse(QTD_APTOS_TOT>=200000,1,0),
         "forcing_variable" = QTD_APTOS_TOT-200000,
         "quadratic" = forcing_variable^2,
         "registered_voters" = QTD_APTOS_TOT,
         "votes_cast_total" = QTD_COMPARECIMENTO,
         "votes_cast_pct" = votes_cast_total/registered_voters,
         "votes_nominal" = QTD_VOTOS_NOMINAIS,
         "votes_nominal_pct" = votes_nominal/registered_voters,
         "valid_votes" = registered_voters - QTD_ABSTENCOES - QTD_VOTOS_NULOS,
         "valid_votes_pct" = valid_votes/registered_voters)

cities_size <- vote_turnout_candidates %>% 
  group_by(ANO_ELEICAO, D) %>% 
  count()

cities_size <- cities_size %>% rename("Election Year" = ANO_ELEICAO, "Have Runoff Elections" = D, "Number of Cities" = n)

# Model quadratic - Number of Candidates -------------------------------------------

rdd_candidates_general <- lm(n ~ D + forcing_variable + 
                               quadratic + quadratic*D, 
                             data=vote_turnout_candidates_quadratic)
# summary(rdd_candidates_general)
## D =1.215e+00  3.831e-01   3.173  0.00157 **

rdd_candidates <- lm(n ~ D + forcing_variable + quadratic + 
                       quadratic*D + factor(ANO_ELEICAO), 
                     data=vote_turnout_candidates_quadratic)
# summary(rdd_candidates)
## D = 1.242e+00  3.821e-01   3.250   0.0012**

rdd_candidates_states <- lm(n ~ D + forcing_variable + quadratic + 
                              quadratic*D + factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                            data=vote_turnout_candidates_quadratic)
# summary(rdd_candidates_states)
## D = 1.222e+00  3.837e-01   3.184  0.00151 **


# Model quadratic - Votes cast pct -------------------------------------------------

rdd_votescast_general <- lm(votes_cast_pct ~ D + forcing_variable + 
                              quadratic + quadratic*D, 
                            data=vote_turnout_candidates_quadratic)
# summary(rdd_votescast_general)
## D = 1.920e-04  9.892e-03   0.019    0.985

rdd_votescast <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                      quadratic*D + factor(ANO_ELEICAO), 
                    data=vote_turnout_candidates_quadratic)
# summary(rdd_votescast)
## D = -4.913e-03  9.207e-03  -0.534    0.594  

rdd_votescast_states <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                             quadratic*D + factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                           data=vote_turnout_candidates_quadratic)
# summary(rdd_votescast_states)
## D = -3.368e-03  8.611e-03  -0.391 0.695859


# Model quadratic - Valid Votes pct ------------------------------------------------

# Olhar impact em votos validos
rdd_validvotes_general <- lm(valid_votes_pct ~ D + forcing_variable + 
                               quadratic + quadratic*D, 
                             data=vote_turnout_candidates_quadratic)
# summary(rdd_validvotes_general)
## 1.498e-02  1.937e-02   0.773    0.440

rdd_validvotes <- lm(valid_votes_pct ~ D + forcing_variable + 
                       quadratic + quadratic*D + factor(ANO_ELEICAO), 
                     data=vote_turnout_candidates_quadratic)
# summary(rdd_validvotes)
## D = 4.455e-03  1.802e-02   0.247    0.805 

rdd_validvotes_states <- lm(valid_votes_pct ~ D + forcing_variable + 
                              quadratic + quadratic*D + factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                            data=vote_turnout_candidates_quadratic)
# summary(rdd_validvotes_states)
## 5.682e-03  1.790e-02   0.317    0.751


# Model quadratic - Nominal Votes --------------------------------------------------

# Olhar impact em nominal votes
rdd_nominalvotes_general <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                 quadratic + quadratic*D, 
                               data=vote_turnout_candidates_quadratic)
# summary(rdd_nominalvotes_general)
# D = 1.342e-02  2.130e-02   0.630    0.529

rdd_nominalvotes <- lm(votes_nominal_pct ~ D + forcing_variable + 
                         quadratic + quadratic*D+ factor(ANO_ELEICAO), 
                       data=vote_turnout_candidates_quadratic)
# summary(rdd_nominalvotes)
# D = 1.478e-03  1.964e-02   0.075    0.940

rdd_nominalvotes_states <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                quadratic + quadratic*D + factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                              data=vote_turnout_candidates_quadratic)
# summary(rdd_nominalvotes_states)
# D = 2.214e-03  1.908e-02   0.116   0.9077


# Data for Yearly Quadratic (global) models  -------------------------------------------

dat2000 <- vote_turnout_candidates_quadratic %>% filter(ANO_ELEICAO==2000)
dat2004 <- vote_turnout_candidates_quadratic %>% filter(ANO_ELEICAO==2004)
dat2008 <- vote_turnout_candidates_quadratic %>% filter(ANO_ELEICAO==2008)
dat2012 <- vote_turnout_candidates_quadratic %>% filter(ANO_ELEICAO==2012)
dat2016 <- vote_turnout_candidates_quadratic %>% filter(ANO_ELEICAO==2016)


# Yearly global models - Impact on N Candidates  -------------------------------------------

## General

rdd_candidates_general_2000 <- lm(n ~ D + forcing_variable + 
                               quadratic + quadratic*D, 
                             data=dat2000)
rdd_candidates_general_2004 <- lm(n ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2004)
rdd_candidates_general_2008 <- lm(n ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2008)
rdd_candidates_general_2012 <- lm(n ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2012)
rdd_candidates_general_2016 <- lm(n ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2016)
summary(rdd_candidates_general_2000)
summary(rdd_candidates_general_2004)
summary(rdd_candidates_general_2008)
summary(rdd_candidates_general_2012)
summary(rdd_candidates_general_2016)
## D2000 = 2.572e+00  1.044e+00   2.465   0.0155 *
## D2004 = 3.076e+00  8.538e-01   3.602 0.000478 ***
## D2008 = 8.651e-01  9.864e-01   0.877    0.382 
## D2012 = 2.291e+00  8.788e-01   2.607   0.0102 * 
## D2016 = -5.889e-01  6.239e-01  -0.944   0.3460


## State Fixed Effects

rdd_candidates_states_2000 <- lm(n ~ D + forcing_variable + quadratic + 
                              quadratic*D + 
                              factor(SIGLA_UF), 
                            data=dat2000)
rdd_candidates_states_2004 <- lm(n ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2004)
rdd_candidates_states_2008 <- lm(n ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2008)
rdd_candidates_states_2012 <- lm(n ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2012)
rdd_candidates_states_2016 <- lm(n ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2016)
summary(rdd_candidates_states_2000)
summary(rdd_candidates_states_2004)
summary(rdd_candidates_states_2008)
summary(rdd_candidates_states_2012)
summary(rdd_candidates_states_2016)

## D = 2.183e+00  1.141e+00   1.914   0.0595 .
## D2004 = 2.837e+00  1.099e+00   2.581   0.0115 *
## D2008 = 1.151e+00  1.106e+00   1.041    0.301
## D2012 = 2.000e+00  9.007e-01   2.221 0.028368 *
## D2016 = -6.672e-01  6.448e-01  -1.035   0.3017

# Yearly global models - Impact on Votes Cast  -------------------------------------------

## General

rdd_votescast_general_2000 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2000)
rdd_votescast_general_2004 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2004)
rdd_votescast_general_2008 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2008)
rdd_votescast_general_2012 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2012)
rdd_votescast_general_2016 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2016)
summary(rdd_votescast_general_2000)
summary(rdd_votescast_general_2004)
summary(rdd_votescast_general_2008)
summary(rdd_votescast_general_2012)
summary(rdd_votescast_general_2016)
## D = 8.143e-03  2.301e-02   0.354    0.724
## D2004 = -4.789e-03  1.433e-02  -0.334    0.739 
## D2008 = 3.026e-02  1.318e-02   2.296   0.0235 *
## D2012 = 4.864e-03  1.578e-02   0.308    0.758
## D2016 = -2.902e-02  2.036e-02  -1.425    0.155


## State Fixed Effects

rdd_votescast_states_2000 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2000)
rdd_votescast_states_2004 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2004)
rdd_votescast_states_2008 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2008)
rdd_votescast_states_2012 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2012)
rdd_votescast_states_2016 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2016)

summary(rdd_votescast_states_2000)
summary(rdd_votescast_states_2004)
summary(rdd_votescast_states_2008)
summary(rdd_votescast_states_2012)
summary(rdd_votescast_states_2016)

## D = 1.004e-02  1.580e-02   0.636  0.52692 
## D2004 = -1.094e-02  1.390e-02  -0.787   0.4334 
## D2008 = 2.884e-02  1.354e-02   2.129   0.0359 *
## D2012 = 1.975e-02  1.458e-02   1.354  0.17831  
## D2016 = -3.128e-02  1.609e-02  -1.943 0.052957 .


# Yearly global models - Impact on Valid Votes -----------------------------------

## General

rdd_validvotes_general_2000 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2000)
rdd_validvotes_general_2004 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2004)
rdd_validvotes_general_2008 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2008)
rdd_validvotes_general_2012 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2012)
rdd_validvotes_general_2016 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2016)
summary(rdd_validvotes_general_2000)
summary(rdd_validvotes_general_2004)
summary(rdd_validvotes_general_2008)
summary(rdd_validvotes_general_2012)
summary(rdd_validvotes_general_2016)
## D2000 = 1.574e-02  2.507e-02   0.628    0.531 
## D2004 = -3.954e-03  2.927e-02  -0.135    0.893
## D2008 = 4.115e-02  2.474e-02   1.663   0.0990 . 
## D2012 = 2.034e-02  5.614e-02   0.362    0.718    
## D2016 = -1.765e-02  3.579e-02  -0.493    0.622 

## State Fixed Effects

rdd_validvotes_states_2000 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2000)
rdd_validvotes_states_2004 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2004)
rdd_validvotes_states_2008 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2008)
rdd_validvotes_states_2012 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2012)
rdd_validvotes_states_2016 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2016)
summary(rdd_validvotes_states_2000)
summary(rdd_validvotes_states_2004)
summary(rdd_validvotes_states_2008)
summary(rdd_validvotes_states_2012)
summary(rdd_validvotes_states_2016)
## D = 9.129e-03  1.650e-02   0.553 0.581671
## D2004 = -2.759e-02  3.644e-02  -0.757    0.451 
## D2008 = 2.475e-02  2.773e-02   0.892    0.374
## D2012 = 5.811e-02  6.249e-02   0.930    0.354   
## D2016 = -1.897e-02  3.428e-02  -0.553    0.580 


# Yearly global Models - Impact on Nominal Votes ---------------------------------

## General

rdd_nominalvotes_general_2000 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2000)
rdd_nominalvotes_general_2004 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2004)
rdd_nominalvotes_general_2008 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2008)
rdd_nominalvotes_general_2012 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2012)
rdd_nominalvotes_general_2016 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2016)
summary(rdd_nominalvotes_general_2000)
summary(rdd_nominalvotes_general_2004)
summary(rdd_nominalvotes_general_2008)
summary(rdd_nominalvotes_general_2012)
summary(rdd_nominalvotes_general_2016)
## D = 1.099e-02  2.461e-02   0.447    0.656
## D2004 = -1.264e-02  2.910e-02  -0.435    0.665
## D2008 = 4.782e-02  2.838e-02   1.685   0.0947 .
## D2012 = 2.435e-02  5.883e-02   0.414    0.680
## D2016 = -2.534e-02  4.022e-02  -0.630    0.529

## State Fixed Effects

rdd_nominalvotes_states_2000 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2000)
rdd_nominalvotes_states_2004 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2004)
rdd_nominalvotes_states_2008 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2008)
rdd_nominalvotes_states_2012 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2012)
rdd_nominalvotes_states_2016 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2016)
summary(rdd_nominalvotes_states_2000)
summary(rdd_nominalvotes_states_2004)
summary(rdd_nominalvotes_states_2008)
summary(rdd_nominalvotes_states_2012)
summary(rdd_nominalvotes_states_2016)

## D = 7.732e-03  1.840e-02   0.420  0.67557 
## D2004 = -3.114e-02  3.683e-02  -0.845    0.400
## D2008 = 2.518e-02  3.159e-02   0.797    0.427
## D2012 =  6.204e-02  6.391e-02   0.971    0.334   
## D2016 = -3.279e-02  3.713e-02  -0.883    0.378





# Data for linear (local strategy) models ----------------------------------------------------------

## Create dataset
vote_turnout_candidates_linear <- 
  left_join(turnout_mayor_general, 
            n_candidates_cities, 
            by=c("ANO_ELEICAO", "CODIGO_MUNICIPIO")) %>% 
  filter(QTD_APTOS_TOT>100000, QTD_APTOS_TOT<300000, NUM_TURNO==1) %>% 
  mutate("D" = ifelse(QTD_APTOS_TOT>=200000,1,0),
         "forcing_variable" = QTD_APTOS_TOT-200000,
         "quadratic" = forcing_variable^2,
         "registered_voters" = QTD_APTOS_TOT,
         "votes_cast_total" = QTD_COMPARECIMENTO,
         "votes_cast_pct" = votes_cast_total/registered_voters,
         "votes_nominal" = QTD_VOTOS_NOMINAIS,
         "votes_nominal_pct" = votes_nominal/registered_voters,
         "valid_votes" = registered_voters - QTD_ABSTENCOES - QTD_VOTOS_NULOS,
         "valid_votes_pct" = valid_votes/registered_voters)

cities_size_local <- vote_turnout_candidates_linear %>% 
  group_by(ANO_ELEICAO, D) %>% 
  count()

cities_size_local <- cities_size_local %>% rename("Election Year" = ANO_ELEICAO, "Have Runoff Elections" = D, "Number of Cities" = n)

# Model linear - Number of Candidates -------------------------------------------

rdd_linear_candidates_general <- lm(n ~ D + forcing_variable + 
                               forcing_variable*D, 
                             data=vote_turnout_candidates_linear)
summary(rdd_linear_candidates_general)
## D = 1.289e+00  2.660e-01   4.844 1.53e-06 ***

rdd_linear_candidates <- lm(n ~ D + forcing_variable + forcing_variable*D + 
                              factor(ANO_ELEICAO), 
                     data=vote_turnout_candidates_linear)
summary(rdd_linear_candidates)
## D = 1.276e+00  2.652e-01   4.811  1.8e-06 ***

rdd_linear_candidates_states <- lm(n ~ D + forcing_variable + forcing_variable*D +
                                     factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                            data=vote_turnout_candidates_linear)
# summary(rdd_linear_candidates_states)
## D = 1.249e+00  2.657e-01   4.702 3.06e-06 ***


# Model linear - Votes cast pct -------------------------------------------------

rdd_linear_votescast_general <- lm(votes_cast_pct ~ D + forcing_variable + 
                              forcing_variable*D, 
                            data=vote_turnout_candidates_linear)
# summary(rdd_linear_votescast_general)
## D = 6.730e-04  6.866e-03   0.098    0.922 

rdd_linear_votescast <- lm(votes_cast_pct ~ D + forcing_variable + forcing_variable*D + factor(ANO_ELEICAO), 
                    data=vote_turnout_candidates_linear)
# summary(rdd_linear_votescast)
## D =  -3.246e-03  6.387e-03  -0.508    0.611   

rdd_linear_votescast_states <- lm(votes_cast_pct ~ D + forcing_variable + 
                             forcing_variable*D + factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                           data=vote_turnout_candidates_linear)
# summary(rdd_linear_votescast_states)
## D = -3.027e-03  5.962e-03  -0.508 0.611728


# Model linear - Valid Votes pct ------------------------------------------------

# Olhar impact em votos validos
rdd_linear_validvotes_general <- lm(valid_votes_pct ~ D + forcing_variable + 
                                      forcing_variable*D, 
                             data=vote_turnout_candidates_linear)
# summary(rdd_linear_validvotes_general)
## 1.175e-02  1.345e-02   0.874    0.383

rdd_linear_validvotes <- lm(valid_votes_pct ~ D + forcing_variable + 
                       forcing_variable*D + factor(ANO_ELEICAO), 
                     data=vote_turnout_candidates_linear)
# summary(rdd_linear_validvotes)
## D = 4.017e-03  1.250e-02   0.321    0.748

rdd_linear_validvotes_states <- lm(valid_votes_pct ~ D + forcing_variable + 
                              forcing_variable*D + factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                            data=vote_turnout_candidates_linear)
# summary(rdd_linear_validvotes_states)
## 2.839e-03  1.239e-02   0.229    0.819 


# Model linear - Nominal Votes --------------------------------------------------

# Olhar impact em nominal votes
rdd_linear_nominalvotes_general <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                        forcing_variable*D, 
                               data=vote_turnout_candidates_linear)
# summary(rdd_linear_nominalvotes_general)
# D = 9.869e-03  1.479e-02   0.668    0.505 

rdd_linear_nominalvotes <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                forcing_variable*D+ factor(ANO_ELEICAO), 
                       data=vote_turnout_candidates_linear)
# summary(rdd_linear_nominalvotes)
# D = 9.355e-04  1.363e-02   0.069    0.945

rdd_linear_nominalvotes_states <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                       forcing_variable*D + factor(ANO_ELEICAO) + factor(SIGLA_UF), 
                              data=vote_turnout_candidates_linear)
# summary(rdd_linear_nominalvotes_states)
# D = -3.231e-04  1.321e-02  -0.024   0.9805


# Data for Yearly Quadratic (global) models  -------------------------------------------

dat2000 <- vote_turnout_candidates_linear %>% filter(ANO_ELEICAO==2000)
dat2004 <- vote_turnout_candidates_linear %>% filter(ANO_ELEICAO==2004)
dat2008 <- vote_turnout_candidates_linear %>% filter(ANO_ELEICAO==2008)
dat2012 <- vote_turnout_candidates_linear %>% filter(ANO_ELEICAO==2012)
dat2016 <- vote_turnout_candidates_linear %>% filter(ANO_ELEICAO==2016)


# Yearly local models - Impact on N Candidates  -------------------------------------------

## General

rdd_linear_candidates_general_2000 <- lm(n ~ D + forcing_variable + 
                                           forcing_variable*D, 
                                  data=dat2000)
rdd_linear_candidates_general_2004 <- lm(n ~ D + forcing_variable + 
                                           forcing_variable*D, 
                                  data=dat2004)
rdd_linear_candidates_general_2008 <- lm(n ~ D + forcing_variable + 
                                           forcing_variable*D, 
                                  data=dat2008)
rdd_linear_candidates_general_2012 <- lm(n ~ D + forcing_variable + 
                                           forcing_variable*D, 
                                  data=dat2012)
rdd_linear_candidates_general_2016 <- lm(n ~ D + forcing_variable + 
                                           forcing_variable*D, 
                                  data=dat2016)
summary(rdd_linear_candidates_general_2000)
summary(rdd_linear_candidates_general_2004)
summary(rdd_linear_candidates_general_2008)
summary(rdd_linear_candidates_general_2012)
summary(rdd_linear_candidates_general_2016)
## D2000 = 2.015e+00  7.167e-01   2.811  0.00597 **
## D2004 = 1.894e+00  6.292e-01   3.010  0.00324 **
## D2008 = 1.500e+00  6.458e-01   2.323   0.0219 *
## D2012 = 1.813e+00  6.219e-01   2.916  0.00416 ** 
## D2016 = 1.367e-01  4.376e-01   0.312  0.75494 


## State Fixed Effects

rdd_linear_candidates_states_2000 <- lm(n ~ D + forcing_variable + forcing_variable*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2000)
rdd_linear_candidates_states_2004 <- lm(n ~ D + forcing_variable + forcing_variable*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2004)
rdd_linear_candidates_states_2008 <- lm(n ~ D + forcing_variable + forcing_variable*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2008)
rdd_linear_candidates_states_2012 <- lm(n ~ D + forcing_variable + forcing_variable*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2012)
rdd_linear_candidates_states_2016 <- lm(n ~ D + forcing_variable + forcing_variable*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2016)
summary(rdd_linear_candidates_states_2000)
summary(rdd_linear_candidates_states_2004)
summary(rdd_linear_candidates_states_2008)
summary(rdd_linear_candidates_states_2012)
summary(rdd_linear_candidates_states_2016)

## D = 2.137e+00  7.928e-01   2.696  0.00865 **
## D2004 =  1.745e+00  8.023e-01   2.175   0.0323 *
## D2008 = 1.433e+00  7.322e-01   1.957   0.0533 .
## D2012 = 1.761e+00  6.341e-01   2.778  0.00641 **
## D2016 = 2.251e-01  4.437e-01   0.507   0.6123  

# NOT REVIEWED Yearly global models - Impact on Votes Cast  -------------------------------------------

## General

rdd_votescast_general_2000 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2000)
rdd_votescast_general_2004 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2004)
rdd_votescast_general_2008 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2008)
rdd_votescast_general_2012 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2012)
rdd_votescast_general_2016 <- lm(votes_cast_pct ~ D + forcing_variable + 
                                   quadratic + quadratic*D, 
                                 data=dat2016)
summary(rdd_votescast_general_2000)
summary(rdd_votescast_general_2004)
summary(rdd_votescast_general_2008)
summary(rdd_votescast_general_2012)
summary(rdd_votescast_general_2016)
## D = 8.143e-03  2.301e-02   0.354    0.724
## D2004 = -4.789e-03  1.433e-02  -0.334    0.739 
## D2008 = 3.026e-02  1.318e-02   2.296   0.0235 *
## D2012 = 4.864e-03  1.578e-02   0.308    0.758
## D2016 = -2.902e-02  2.036e-02  -1.425    0.155


## State Fixed Effects

rdd_votescast_states_2000 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2000)
rdd_votescast_states_2004 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2004)
rdd_votescast_states_2008 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2008)
rdd_votescast_states_2012 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2012)
rdd_votescast_states_2016 <- lm(votes_cast_pct ~ D + forcing_variable + quadratic + 
                                  quadratic*D + 
                                  factor(SIGLA_UF), 
                                data=dat2016)

summary(rdd_votescast_states_2000)
summary(rdd_votescast_states_2004)
summary(rdd_votescast_states_2008)
summary(rdd_votescast_states_2012)
summary(rdd_votescast_states_2016)

## D = 1.004e-02  1.580e-02   0.636  0.52692 
## D2004 = -1.094e-02  1.390e-02  -0.787   0.4334 
## D2008 = 2.884e-02  1.354e-02   2.129   0.0359 *
## D2012 = 1.975e-02  1.458e-02   1.354  0.17831  
## D2016 = -3.128e-02  1.609e-02  -1.943 0.052957 .


# NOT REVIEWED Yearly global models - Impact on Valid Votes -----------------------------------

## General

rdd_validvotes_general_2000 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2000)
rdd_validvotes_general_2004 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2004)
rdd_validvotes_general_2008 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2008)
rdd_validvotes_general_2012 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2012)
rdd_validvotes_general_2016 <- lm(valid_votes_pct ~ D + forcing_variable + 
                                    quadratic + quadratic*D, 
                                  data=dat2016)
summary(rdd_validvotes_general_2000)
summary(rdd_validvotes_general_2004)
summary(rdd_validvotes_general_2008)
summary(rdd_validvotes_general_2012)
summary(rdd_validvotes_general_2016)
## D2000 = 1.574e-02  2.507e-02   0.628    0.531 
## D2004 = -3.954e-03  2.927e-02  -0.135    0.893
## D2008 = 4.115e-02  2.474e-02   1.663   0.0990 . 
## D2012 = 2.034e-02  5.614e-02   0.362    0.718    
## D2016 = -1.765e-02  3.579e-02  -0.493    0.622 

## State Fixed Effects

rdd_validvotes_states_2000 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2000)
rdd_validvotes_states_2004 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2004)
rdd_validvotes_states_2008 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2008)
rdd_validvotes_states_2012 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2012)
rdd_validvotes_states_2016 <- lm(valid_votes_pct ~ D + forcing_variable + quadratic + 
                                   quadratic*D + 
                                   factor(SIGLA_UF), 
                                 data=dat2016)
summary(rdd_validvotes_states_2000)
summary(rdd_validvotes_states_2004)
summary(rdd_validvotes_states_2008)
summary(rdd_validvotes_states_2012)
summary(rdd_validvotes_states_2016)
## D = 9.129e-03  1.650e-02   0.553 0.581671
## D2004 = -2.759e-02  3.644e-02  -0.757    0.451 
## D2008 = 2.475e-02  2.773e-02   0.892    0.374
## D2012 = 5.811e-02  6.249e-02   0.930    0.354   
## D2016 = -1.897e-02  3.428e-02  -0.553    0.580 


# NOT REVIEWED Yearly global Models - Impact on Nominal Votes ---------------------------------

## General

rdd_nominalvotes_general_2000 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2000)
rdd_nominalvotes_general_2004 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2004)
rdd_nominalvotes_general_2008 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2008)
rdd_nominalvotes_general_2012 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2012)
rdd_nominalvotes_general_2016 <- lm(votes_nominal_pct ~ D + forcing_variable + 
                                      quadratic + quadratic*D, 
                                    data=dat2016)
summary(rdd_nominalvotes_general_2000)
summary(rdd_nominalvotes_general_2004)
summary(rdd_nominalvotes_general_2008)
summary(rdd_nominalvotes_general_2012)
summary(rdd_nominalvotes_general_2016)
## D = 1.099e-02  2.461e-02   0.447    0.656
## D2004 = -1.264e-02  2.910e-02  -0.435    0.665
## D2008 = 4.782e-02  2.838e-02   1.685   0.0947 .
## D2012 = 2.435e-02  5.883e-02   0.414    0.680
## D2016 = -2.534e-02  4.022e-02  -0.630    0.529

## State Fixed Effects

rdd_nominalvotes_states_2000 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2000)
rdd_nominalvotes_states_2004 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2004)
rdd_nominalvotes_states_2008 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2008)
rdd_nominalvotes_states_2012 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2012)
rdd_nominalvotes_states_2016 <- lm(votes_nominal_pct ~ D + forcing_variable + quadratic + 
                                     quadratic*D + 
                                     factor(SIGLA_UF), 
                                   data=dat2016)
summary(rdd_nominalvotes_states_2000)
summary(rdd_nominalvotes_states_2004)
summary(rdd_nominalvotes_states_2008)
summary(rdd_nominalvotes_states_2012)
summary(rdd_nominalvotes_states_2016)

## D = 7.732e-03  1.840e-02   0.420  0.67557 
## D2004 = -3.114e-02  3.683e-02  -0.845    0.400
## D2008 = 2.518e-02  3.159e-02   0.797    0.427
## D2012 =  6.204e-02  6.391e-02   0.971    0.334   
## D2016 = -3.279e-02  3.713e-02  -0.883    0.378



