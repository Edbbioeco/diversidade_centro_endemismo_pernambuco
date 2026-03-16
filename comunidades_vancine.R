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
