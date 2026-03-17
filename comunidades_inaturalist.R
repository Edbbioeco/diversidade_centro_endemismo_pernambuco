# Pacotes ----

library(sf)

library(tidyverse)

library(rinat)

library(readxl)

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

### Delimitando o limite ----

limite_cep <- grade |>
  sf::st_bbox()

limite_cep

### Importando ----

inat <- rinat::get_inat_obs(taxon_name = "Amphibia",
                            bounds = c(limite_cep["ymin"],
                                       limite_cep["xmin"],
                                       limite_cep["ymax"],
                                       limite_cep["xmax"]))

### Visualizando -----

inat

inat |> dplyr::glimpse()

inat |>
  rinat::inat_map(map = "world") +
  coord_sf(xlim = c(limite_cep["xmin"], limite_cep["xmax"]),
           ylim = c(limite_cep["ymin"], limite_cep["ymax"]))

### Transformando um shapefile ----

ocorrencias_s
