# Pacotes ----

library(sf)

library(tidyverse)

library(rinat)

library(writexl)

# Dados ----

## Grade ----

### Importando ----

grade <- sf::st_read("cep_grade.shp")

### Visualizando ----

grade

ggplot() +
  geom_sf(data = grade, color = "forestgreen", fill = "transparent")

## Registros de ocorrência do iNaturalist ----
