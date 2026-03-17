# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

registros <- readxl::read_excel("registros.xlsx")

## Visualizando ----

registros

registros |> dplyr::glimpse()


matriz_trat |>
  dplyr::select(7:103) |>
  names()
