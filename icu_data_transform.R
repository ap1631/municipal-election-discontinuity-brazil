install.packages("stringr")
library(stringr)
library(tidyverse)
library(readxl)

data_icubedseptember <- read_xls("covidicuseptember_transform.xls")
data_icubedseptember$`Quantidade_N„o_SUS` <- ifelse(data_icubedseptember$`Quantidade_N„o_SUS`== "-", 0, data_icubedseptember$`Quantidade_N„o_SUS`)
data_icubedseptember$`Quantidade_SUS` <- ifelse(data_icubedseptember$`Quantidade_SUS`== "-", 0, data_icubedseptember$`Quantidade_SUS`)

data_icubedmarch <- read_xls("covidicumarch_transform.xls")
data_icubedmarch$`Quantidade_N„o_SUS` <- ifelse(data_icubedmarch$`Quantidade_N„o_SUS`== "-", 0, data_icubedmarch$`Quantidade_N„o_SUS`)
data_icubedmarch$`Quantidade_SUS` <- ifelse(data_icubedmarch$`Quantidade_SUS`== "-", 0, data_icubedmarch$`Quantidade_SUS`)

data_covid_delta <- left_join(data_icubedseptember, data_icubedmarch, by="codigo_ibge")

data_covid_delta <-
  data_covid_delta %>% 
  mutate(
    "delta" = Quantidade_existente.x - Quantidade_existente.y,
  )


summary(data_covid_delta$delta)



data_covid_delta$delta_private <- 
  data_covid_delta$`Quantidade_N„o_SUS.x`- data_covid_delta$`Quantidade_N„o_SUS.y`

data_covid_delta$delta_public <- 
  data_covid_delta$Quantidade_SUS.x - data_covid_delta$`Quantidade_SUS`.y
