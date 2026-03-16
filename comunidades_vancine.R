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

# Comunidades ----

## Criando um shapefile das ocorrências ----

oc_vancine_shp <- oc_vancine_trat |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = grade_cep |> sf::st_crs())

oc_vancine_shp

ggplot() +
  geom_sf(data = grade_cep) +
  geom_sf(data = oc_vancine_shp)

## Espécies por grade ----

### Intersecção ----

oc_vancine_inter <- grade_cep |>
  sf::st_join(oc_vancine_shp,
              join = st_intersects) |>
  dplyr::filter(!species |> is.na()) |>
  tibble::as_tibble() |>
  dplyr::select(FID, species) |>
  dplyr::mutate(presence = 1,
                Source = "GBIF") |>
  dplyr::bind_cols(grade_cep |>
                     sf::st_join(oc_vancine_shp,
                                 join = st_intersects) |>
                     dplyr::filter(!is.na(species)) |>
                     sf::st_centroid() |>
                     sf::st_coordinates() |>
                     tibble::as_tibble() |>
                     dplyr::select(1:2) |>
                     dplyr::rename("Longitude" = X,
                                   "Latitude" = Y))

oc_vancine_inter
