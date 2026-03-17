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
  dplyr::mutate(`vertlif present` = NA,
                `present as` = NA,
                replaced = NA,
                `replaced species` = NA)

matriz_sin

## Exportando ----

matriz_sin |>
  writexl::write_xlsx("matriz_sinonimios.xlsx")
