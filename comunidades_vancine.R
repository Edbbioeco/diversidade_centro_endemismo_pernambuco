# Pacotes ----

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Ocorrências ----

### Importando ----

oc_vancine <- readr::read_csv("C:/Users/LENOVO/OneDrive/Documentos/curso_diversidade_taxonomica_simpzoo/ATLANTIC_AMPHIBIANS_sites.csv")

### Visualizando -----

oc_vancine

oc_vancine |> dplyr::glimpse()

## Grade ----

### Importando ----

grade_cep <- sf::st_read("cep_grade.shp")

### Visualizando ----

grade_cep

grade_cep |>
  ggplot() +
  geom_sf(color = "black", fill = "green4")
