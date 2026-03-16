# Pacotes ----

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Identificação das espécies ----

### Importando ----

id_sps <- readr::read_csv("C:/Users/LENOVO/OneDrive/Documentos/curso_diversidade_taxonomica_simpzoo/ATLANTIC_AMPHIBIANS_species.csv")

### visualizando ----

id_sps

id_sps |> dplyr::glimpse()

### Tratando ----

id_sps_trat <- id_sps |>
  dplyr::select(id, valid_name) |>
  dplyr::filter(!valid_name |> is.na()) |>
  dplyr::rename("species" = valid_name)

id_sps_trat

## Ocorrências ----

### Importando ----

oc_vancine <- readr::read_csv("C:/Users/LENOVO/OneDrive/Documentos/curso_diversidade_taxonomica_simpzoo/ATLANTIC_AMPHIBIANS_sites.csv")

### Visualizando -----

oc_vancine

oc_vancine |> dplyr::glimpse()

### Tratando ----

oc_vancine_trat <- oc_vancine |>
  dplyr::select(id, longitude, latitude) |>
  dplyr::left_join(id_sps_trat,
                   by = "id") |>
  dplyr::filter(!longitude |> is.na() & !latitude |> is.na())

oc_vancine_trat

oc_vancine_trat |> dplyr::glimpse()

## Grade ----

### Importando ----

grade_cep <- sf::st_read("cep_grade.shp")

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")
