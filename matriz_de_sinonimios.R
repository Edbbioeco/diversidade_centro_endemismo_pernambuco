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

# Matriz ----

## Montando a matriz ----

matriz_sin <- registros |>
  dplyr::select(2:3) |>
  dplyr::mutate(synonym = NA,
                `vertlif present` = NA,
                `present as` = NA,
                replaced = NA,
                `replaced species` = NA)

matriz_sin

## Exportando ----

mat













matriz_trat |>
  dplyr::select(7:103) |>
  names()
