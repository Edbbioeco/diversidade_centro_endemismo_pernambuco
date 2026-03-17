# Pacotes ----

library(sf)

library(tidyverse)

library(readxl)

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

## Dados das famílias ----

### Importando ----

registro_gbif <- readxl::read_xlsx("registros_gbif.xlsx")

### Visualizando ----

registro_gbif

registro_gbif |> dplyr::glimpse()

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

inat_sf <- inat |>
  dplyr::filter(dplyr::across(.cols = dplyr::contains("itude"),
                              .fns = ~!is.na(.))) |>
  dplyr::select("species" = scientific_name,
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
                Source = "Vancine") |>
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

oc_vancine_inter

### Matriz ----

oc_vancine_inter |>
  tidyr::pivot_wider(names_from = species,
                     values_from = presence,
                     values_fn = function(x) 1,
                     values_fill = 0)

### Exportando ----

oc_vancine_inter |>
  openxlsx::write.xlsx("registros_vancine.xlsx")
