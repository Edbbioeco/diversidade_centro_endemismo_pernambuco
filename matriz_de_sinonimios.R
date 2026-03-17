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
  dplyr::distinct(species, .keep_all = TRUE) |>
  dplyr::mutate(`vertlife name` = NA,
                `vertlife present` = NA,
                replaced = NA,
                `replaced species` = NA) |>
  dplyr::arrange(family, species)

matriz_sin

## Exportando ----

matriz_sin |>
  writexl::write_xlsx("matriz_sinonimios.xlsx")
