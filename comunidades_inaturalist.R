# Pacotes ----

library(sf)

library(tidyverse)

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

inat <- readr::read_csv("inaturalist.csv")

### Visualizando -----

inat

inat |> dplyr::glimpse()

### Transformando um shapefile ----

inat_sf <- inat |>
  dplyr::mutate(palavras = scientific_name |> stringr::str_count("\\S+")) |>
  dplyr::filter(!longitude |> is.na(),
                !latitude |> is.na(),
                palavras > 1) |>
  dplyr::select("family" = taxon_family_name,
                "species" = scientific_name,
                longitude,
                latitude) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = grade |> sf::st_crs())

inat_sf

ggplot() +
  geom_sf(data = grade, color = "forestgreen", fill = "transparent") +
  geom_sf(data = inat_sf)

# Comunidades ----

## Espécies por grade ----

### Intersecção ----

oc_inat_inter <- grade |>
  sf::st_join(inat_sf,
              join = st_intersects) |>
  dplyr::filter(!species |> is.na()) |>
  tibble::as_tibble() |>
  dplyr::select(FID, family, species) |>
  dplyr::mutate(presence = 1,
                Source = "iNaturalist") |>
  dplyr::bind_cols(grade |>
                     sf::st_join(inat_sf,
                                 join = st_intersects) |>
                     dplyr::filter(!is.na(species)) |>
                     sf::st_centroid() |>
                     sf::st_coordinates() |>
                     tibble::as_tibble() |>
                     dplyr::select(1:2) |>
                     dplyr::rename("Longitude" = X,
                                   "Latitude" = Y))

oc_inat_inter

### Matriz ----

oc_inat_inter |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fn = function(x) 1,
                     values_fill = 0)

### Exportando ----

oc_inat_inter |>
  openxlsx::write.xlsx("registros_inaturalist.xlsx")
